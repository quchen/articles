{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module HindleyMilner where

import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.Trans.State
import Data.String
import Control.Monad.Trans.Except
import Control.Monad.Trans
import Control.Monad.Identity

newtype Name = Name Text
    deriving (Eq, Ord)

instance IsString Name where
    fromString = Name . T.pack

data Exp = EVar Name         -- x
         | EApp Exp Exp      -- f x
         | EAbs Name Exp     -- \x -> e
         | ELet Name Exp Exp -- let x = e in e'

-- | A monotype is an unquantified/unparametric type. Monotypes are the inner
-- building blocks of all types.
--
-- In formal notation, 'MType's are often called τ (tau) types.
data MType = TVar Name        -- a
           | TFun MType MType -- a -> b

-- | A polytype is a monotype universally quantified over a number of type
--   variables.
--
-- Example: in a definition @id :: forall a. a -> a@, the @a@ after the
-- ∀ ("forall") is the collection of type variables, and -- @a -> a@ is the
-- 'MType' quantified over.
--
-- In formal notation, 'PType's are often called σ (sigma) types.
data PType = Forall (Set Name) MType

-- | The environment consists of all the values in scope, and their
-- associated polytypes.
--
-- Conceptually, the environment also contains all the things you can form by
-- combining its elements, but this behaviour is modeled by the inference rules
-- and not by the environment itself.
--
-- In Haskell terms, the environment consists of all the things you currently
-- have available. So if you import the Prelude, your environment consists of
--
-- @
-- id        →  ∀a. a→a
-- map       →  ∀a b. (a→b) → [a] → [b]
-- putStrLn  →  ∀∅. String → IO ()
-- …
-- @
newtype Env = Env (Map Name PType)


--------------------------------------------------------------------------------


class Pretty a where
    ppr :: a -> Text


instance Pretty Name where
    ppr (Name n) = n

data Parenthesize = Parentheses | NoParentheses

instance Pretty MType where
    ppr = go NoParentheses
      where
        go _ (TVar (Name name)) = name
        go parenthesize (TFun a b) =
            let lhs = go Parentheses a
                rhs = go NoParentheses b
            in case parenthesize of
                Parentheses   -> "(" <> lhs <> ") → " <> rhs
                NoParentheses -> lhs <> " → " <> rhs

instance Pretty PType where
    ppr (Forall xs mType) = "∀" <> universals <> ". " <> ppr mType
      where
        universals = T.intercalate " " (map ppr (S.toList xs))

instance Pretty Env where
    ppr (Env env) = "Γ = " <> T.intercalate ", " pprBindings
      where
        bindings = M.assocs env
        pprBinding (name, pType) = ppr name <> " ≡ " <> ppr pType
        pprBindings = map pprBinding bindings


--------------------------------------------------------------------------------


-- | A substitution is a mapping from type variables to 'MType's. Applying a
--   substitution means applying those replacements to a value.
newtype Subst = Subst (Map Name MType)

empty :: Subst
empty = Subst M.empty

-- | Combine two substitutions by merging them, after applying all suitable
-- substitutions of the left to the right hand side.
compose :: Subst -> Subst -> Subst
compose subst1@(Subst s1) (Subst s2) = Subst (s1 `M.union` s2')
    where s2' = fmap (substMType subst1) s2


-- Monoid laws:
--
-- First note that fmap does not change a Map's keys, hence it does not affect
-- which elements are discarded by the union. Therefore it distributes over
-- unions,
--
-- fmap f m1 `M.union` fmap f m2 = fmap f (m1 `M.union` m2)
--
-- Furthermore, substitutions are idempotent, i.e. applying a substitution to
-- itself does nothing.
--
-- In particular, this means that
--
--   Subst s1 <> Subst s2
-- = Subst (s1 `M.union` s2') where s2' = fmap (substMType (Subst s1)) s2                (inline the where)
-- = Subst (fmap (substMType (Subst s1)) s1 `M.union` fmap (substMType (Subst s1)) s2)   (idempotence of substitutions)
-- = Subst (fmap (substMType (Subst s1)) (s1 `M.union` s2))                              (distributivity)
--
-- Now we can prove the laws. As a shorthand, write f = substMType . Subst.
--
-- * Left identity law
--
--   ** Helper:
--
--     Goal: substMType mempty a = a
--     (Proof via structural induction)
--
--     Case 1: TVar
--           substMType mempty (TVar a)
--         = let (Subst s') = mempty in M.findWithDefault (TVar a) a s'
--         = M.findWithDefault (TVar a) a M.empty
--         = TVar a
--     Case 2: TFun
--           substMType mempty (TFun f x)
--         = TFun (substMType mempty f) (substMType mempty x)
--     Done
--
--   mempty <> Subst s2                                        (corolarry from above)
-- = Subst (fmap (substMType mempty) (M.empty `M.union` s2))   (M.empty is the identity of unions)
-- = Subst (fmap (substMType mempty) s2)                       (Helper from above)
-- = Subst (fmap id s2)                                        (Functor law)
-- = Subst s2
--
--
-- * Right identity law
--
--   Subst s1 <> mempty                           (corollary from above)
-- = Subst (fmap (f s1) (s1 `M.union` M.empty))   (M.empty is the identity of unions)
-- = Subst (fmap (f s1) s1)                       (Idempotence)
-- = Subst s1
--
--
-- * Distributivity law
--
--   Subst s1 <> (Subst s2 <> Subst s3)                                                                     (inline mappend)
-- = Subst s1 <> Subst (s2 `M.union` fmap (f s2) s3)                                                        (inline mappend)
-- = Subst (s1 `M.union` fmap (f s1) (Subst (s2 `M.union` fmap (f s2) s3)))                                 (idempotence)
-- = Subst (fmap (f s1) s1 `M.union` fmap (f s1) (Subst (s2 `M.union` fmap (f s2) s3)))                     (distributivity)
-- = Subst (fmap (f s1) (s1 `M.union` Subst (s2 `M.union` fmap (f s2) s3)))                                 (idempotence)
-- = Subst (fmap (f s1) (s1 `M.union` Subst (fmap (f s2) s2 `M.union` fmap (f s2) s3)))                     (distributivity)
-- = Subst (fmap (f s1) (s1 `M.union` Subst (fmap (f s2) (s2 `M.union` s3))))                               (????)
--
--   (Subst s1 <> Subst s2) <> Subst s3                                                                     (inline mappend)
-- = Subst (s1 `M.union` fmap (f s1) s2) <> Subst s3                                                        (inline mappend)
-- = Subst ((s1 `M.union` fmap (f s1) s2) `M.union` fmap (f (s `M.union` fmap (f s2) s3))) s3)              (associativity of union)
-- = Subst (s1 `M.union` fmap (f s1) s2 `M.union` fmap (f (s `M.union` fmap (f s2) s3))) s3)                (idempotence)
-- = Subst (fmap (f s1) s1 `M.union` fmap (f s1) s2 `M.union` fmap (f (s `M.union` fmap (f s2) s3))) s3)    (distributivity)
-- = Subst (fmap (f s1) (s1 `M.union` s2) `M.union` fmap (f (s `M.union` fmap (f s2) s3))) s3)              (idempotence)
-- = Subst (fmap (f s1) (s1 `M.union` s2) `M.union` fmap (f (fmap (f s2) s2 `M.union` fmap (f s2) s3))) s3) (distributivity)
-- = Subst (fmap (f s1) (s1 `M.union` s2) `M.union` fmap (f (fmap (f s2) (s2 `M.union` s3)))) s3)           (distributivity)


  Subst s1 <> (Subst s2 <> Subst s3)
= Subst s1 <> Subst (s2 `M.union` fmap (f s2) s3)
= Subst (s1 `M.union` fmap (f s1) (s2 `M.union` fmap (f s2) s3))
= Subst (s1 `M.union` (fmap (f s1) s2 `M.union` fmap (f s1) (fmap (f s2) s3)))
= Subst (s1 `M.union` (fmap (f s1) s2 `M.union` fmap (f s1 . f s2) s3))
= Subst (s1 `M.union` fmap (f s1) s2 `M.union` fmap (f s1 . f s2) s3)

  (Subst s1 <> Subst s2) <> Subst s3
= Subst (s1 `M.union` fmap (f s1) s2) <> Subst s3
= Subst ((s1 `M.union` fmap (f s1) s2) `M.union` fmap (f (s1 `M.union` fmap (f s1) s2)) s3)
= Subst (s1 `M.union` fmap (f s1) s2 `M.union` fmap (f (s1 `M.union` fmap (f s1) s2)) s3)


--------------------------------------------------------------------------------


-- | The free variables of an 'MType'. This is simply the collection of all the
-- individual type variables occurring inside of it.
freeMType :: MType -> Set Name
freeMType = \case
    TVar a   -> [a]
    TFun f x -> freeMType f <> freeMType x

-- | Apply a substitution to all known variables contained in an 'MType'.
substMType :: Subst -> MType -> MType
substMType s = \case
    TVar a -> let Subst s' = s
              in M.findWithDefault (TVar a) a s'
    TFun f x -> TFun (substMType s f) (substMType s x)


--------------------------------------------------------------------------------


-- | The free variables of a 'PType' are the free variables of the contained
-- 'MType', except those universally quantified.
freePType :: PType -> Set Name
freePType (Forall vars mType) = freeMType mType `S.difference` vars

-- | Apply a substitution to a 'PType', replacing all known variables in the
--   contained 'MType' except the ones universally quantified.
--
--   Invariant: the quantified variables are not changed by the operation.
substPType :: Subst -> PType -> PType
substPType (Subst subst) (Forall quantified mType) =
    let quantified' = M.fromSet (const ()) quantified
        subst' = Subst (subst `M.difference` quantified')
    in Forall quantified (substMType subst' mType)


--------------------------------------------------------------------------------


freeEnv :: Env -> Set Name
freeEnv (Env env) = let allPTypes = M.elems env
                    in S.unions (map freePType allPTypes)


substEnv :: Subst -> Env -> Env
substEnv s (Env env) = Env (M.map (substPType s) env)


--------------------------------------------------------------------------------


newtype Infer a = Infer { runInfer :: ExceptT Text (State Integer) a }
    deriving (Functor, Applicative, Monad)

throw :: Text -> Infer a
throw err = Infer (ExceptT (StateT (\s -> Identity (Left err, s))))

cannotUnify :: MType -> MType -> Infer a
cannotUnify a b = throw ("Cannot unify " <> ppr a <> " and " <> ppr b)

occursCheck :: Name -> MType -> Infer a
occursCheck (Name n) a =
    throw ("Occurs check failed for " <> n <> " in " <> ppr a)

-- | Generate a fresh 'Name' in a type 'Infer'ence context.
fresh :: Infer MType
fresh = do
    name <- Infer (lift get)
    let pretty = "t" <> T.pack (show name)
    pure (TVar (Name pretty))

-- | Generalize an 'MType' to a 'PType' by universally quantifying over all
-- the type variables contained in it, except those already mentioned in the
-- environment.
--
-- @
-- generalize(Γ, τ) = ∀{α}. σ
--     where {α} = free(τ) - free(Γ)
-- @
generalize :: Env -> MType -> PType
generalize env mType = Forall as mType
    where as = freeMType mType `S.difference` freeEnv env

-- | Look up the 'PType' of a 'Name' in the 'Env'ironment.
lookupEnv :: Env -> Name -> Infer PType
lookupEnv (Env env) name@(Name n) = case M.lookup name env of
    Just x -> pure x
    Nothing -> throw ("Unknown identifier " <> n)

-- | Add a new binding to the environment.
--
-- @
-- Γ, x:σ  ≡  extendEnv Γ x σ
-- @
extendEnv :: Env -> Name -> PType -> Env
extendEnv (Env env) name pType = Env (M.insert name pType env)

-- | Bind all quantified variables of a 'PType' to 'fresh' type variables.
instantiate :: PType -> Infer MType
instantiate (Forall as t) = do
    subst <- fmap Subst (sequenceA (M.fromSet (const fresh) as))
    pure (substMType subst t)


--------------------------------------------------------------------------------


-- | Infer the type of an 'Exp' in an 'Env', resulting in the 'Exp's 'MType'
-- along with a substitution that has to be done in order to reach this goal.
--
-- This is widely known as /Algorithm W/.
infer :: Env -> Exp -> Infer (Subst, MType)


-- Inferring the type of a variable is done via
--
-- @
-- x:σ ∈ Γ   τ = inst(σ)
-- --------------------- [Var]
--      Γ ⊢ x:τ
-- @
--
-- which simply means that if @x@ is available in polymorphic form, then we have
-- it available in all possible instantiations of that σ type.
--
-- For example, if you have a top-level definition @id :: ∀a. a -> a@ available
-- in Haskell, you also have
infer env (EVar name) = do
    sigma <- lookupEnv env name -- x:σ ∈ Γ
    tau <- instantiate sigma    -- τ = inst(σ)
                                -- -----------
    pure (empty, tau)           -- Γ ⊢ x:τ


-- Function application captures the fact that if we have a function and an
-- argument we can give to that function, we also have the result value
-- available to us.
--
-- @
-- Γ ⊢ f : fτ   Γ ⊢ x : xτ   fxτ = fresh   unify(fτ, xτ → fxτ)
-- ----------------------------------------------------------- [App]
--                       Γ ⊢ f x : fxτ
-- @
--
-- So if we have both @f:τ→τ'@ and @x:τ@ available, we also have access to
-- @f x : τ'@.
--
infer env (EApp f x) = do
    (s1, fTau) <- infer env f                          -- f : fτ
    (s2, xTau) <- infer (substEnv s1 env) x            -- x : xτ
    fxTau <- fresh                                     -- fxτ = fresh
    s3 <- unify (substMType s2 fTau) (TFun xTau fxTau) -- unify'(fτ, xτ → fxτ)
    let s = s3 `compose` s2 `compose` s1               -- --------------------
    pure (s, substMType s3 fxTau)                      -- f x : fxτ


-- Lambda abstraction is based on the fact that when we introduce a new
-- variable, the resulting lambda body maps from that variable's type to the
-- type of the body.
--
-- The typing rule is
--
-- @
-- τ = fresh   σ = gen(Γ, τ)   Γ, x:σ ⊢ e:τ'
-- ----------------------------------------- [Abs]
--              Γ ⊢ λx.e : τ→τ'
-- @
--
-- Here, @Γ, x:τ@ is @Γ@ extended by one additional mapping, namely @x:τ@.
infer env (EAbs x e) = do
    tau <- fresh                           -- τ = fresh
    let sigma = generalize env tau         -- σ = gen(Γ, τ)
        env' = extendEnv env x sigma       -- Γ, x:σ …
    (s, tau') <- infer env' e              --        … ⊢ e:τ'
                                           -- ---------------
    pure (s, TFun (substMType s tau) tau') -- λx.e : τ→τ'


-- A let binding allows extending the environment with new bindings in a
-- principled manner. To do this, we first have to typecheck the expression
-- to be introduced. The result of this is then generalized to a PType, since
-- let bindings introduce new polymorphic values, and then added to the
-- environment. Now we can finally typecheck the body of the "in" part of the
-- let binding.
--
-- @
-- Γ ⊢ e:τ   σ = gen(Γ,τ)   Γ, x:σ ⊢ e':τ'
-- --------------------------------------- [Let]
--         Γ ⊢ let x = e in e' : τ'
-- @
infer env (ELet x e e') = do
    (s1, tau) <- infer env e           -- Γ ⊢ e:τ
    let env' = substEnv s1 env
    let sigma = generalize env' tau    -- σ = gen(Γ,τ)
    let env'' = extendEnv env' x sigma -- Γ, x:σ
    (s2, tau') <- infer env'' e'       -- Γ ⊢ …
                                       -- --------------------------
    pure (s2 `compose` s1, tau')       --     … let x = e in e' : τ'



--------------------------------------------------------------------------------


-- | The unification of two 'MType's is the most general substituion that can
--   be applied to both of them in order to yield the same result.
--
--   In other words, find a 'Subst' @S@ for two types @a@ and @b@ such that
--
--   * S(a) = S(b)
--   * There is no S' other than the identity such that S'(S(a)) = S'(S(b))
unify :: MType -> MType -> Infer Subst
unify (TFun a b) (TFun x y) = do
    subst1 <- unify a x
    subst2 <- unify (substMType subst1 b) (substMType subst1 y)
    pure (compose subst1 subst2)
unify (TVar v) x = bind v x
unify x (TVar v) = bind v x

-- | Build a 'Subst'itution that binds a 'Name' to an 'MType'.
--
-- This is what happens when a type variable is unified with another 'MType':
-- the variable is simply bound to that 'MType'.
bind :: Name -> MType -> Infer Subst
-- TVar unified with itself
bind name (TVar v) | name == v = pure empty
-- Variable is already bound
bind name mType | name `S.member` freeMType mType = occursCheck name mType
bind name mType = pure (Subst (M.singleton name mType))
