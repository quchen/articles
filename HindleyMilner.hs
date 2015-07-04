{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

-- | This module is an extensively documented walkthrough for typechecking a
-- basic functional language using the Hindley-Damas-Milner algorithm.
--
-- It has the following features:
--
-- - This module can be run in GHCi.
-- - The source is written in literate programming style, so you can almost
--   read it from top to bottom, minus some early references to later.
-- - The Haddock output yields a nice overview.

module HindleyMilner where

import           Control.Monad.Identity
import           Control.Monad.Trans
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Monoid
import           Data.Set                   (Set)
import qualified Data.Set                   as S
import           Data.String
import           Data.Text                  (Text)
import qualified Data.Text                  as T






-- #############################################################################
-- #############################################################################
-- * Preliminaries
-- #############################################################################
-- #############################################################################



-- #############################################################################
-- ** Prettyprinting
-- #############################################################################



-- | A prettyprinter class. Similar to 'Show', but with a focus on having
-- human-readable output as opposed to being valid Haskell.
class Pretty a where
    ppr :: a -> Text



-- | A 'name' is an identifier in the language we're going to typecheck.
-- Variables on both the term and type level have 'Name's, for example.
newtype Name = Name Text
    deriving (Eq, Ord)

instance IsString Name where
    fromString = Name . T.pack

instance Pretty Name where
    ppr (Name n) = n





-- #############################################################################
-- ** Monotypes
-- #############################################################################



-- | A monotype is an unquantified/unparametric type. Monotypes are the inner
-- building blocks of all types. Examples of monotypes are @Int@, @a@, @a -> b@.
--
-- In formal notation, 'MType's are often called τ (tau) types.
data MType = TVar Name        -- a
           | TFun MType MType -- a -> b
           -- TODO: Add terms that don't necessarily typecheck :-)

instance Pretty MType where
    ppr = go False
      where
        go _ (TVar (Name name)) = name
        go parenthesize (TFun a b)
            | parenthesize = "(" <> lhs <> ") → " <> rhs
            | otherwise    = lhs <> " → " <> rhs
            where lhs = go True a
                  rhs = go False b



-- | The free variables of an 'MType'. This is simply the collection of all the
-- individual type variables occurring inside of it. For example, the free
-- variables of @a -> b@ are @a@ and @b@.
freeMType :: MType -> Set Name
freeMType = \case
    TVar a   -> [a]
    TFun f x -> freeMType f <> freeMType x



-- | Apply a substitution to all known variables contained in an 'MType'.
-- Variables not mentioned are left unchanged.
substMType :: Subst -> MType -> MType
substMType s = \case
    TVar a -> let Subst s' = s
              in M.findWithDefault (TVar a) a s'
    TFun f x -> TFun (substMType s f) (substMType s x)





-- #############################################################################
-- ** Polytypes
-- #############################################################################


-- | A polytype is a monotype universally quantified over a number of type
-- variables.
--
-- Example: in a definition @id :: forall a. a -> a@, the @a@ after the
-- ∀ ("forall") is the collection of type variables, and @a -> a@ is the
-- 'MType' quantified over.
--
-- In formal notation, 'PType's are often called σ (sigma) types.
data PType = Forall (Set Name) MType

instance Pretty PType where
    ppr (Forall xs mType) = "∀" <> universals <> ". " <> ppr mType
      where
        universals = T.intercalate " " (map ppr (S.toList xs))



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





-- #############################################################################
-- ** The environment
-- #############################################################################



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

instance Pretty Env where
    ppr (Env env) = "Γ = " <> T.intercalate ", " pprBindings
      where
        bindings = M.assocs env
        pprBinding (name, pType) = ppr name <> " ≡ " <> ppr pType
        pprBindings = map pprBinding bindings



-- | The free variables of an 'Env'ironment are all the free variables of the
-- 'PType's it contains.
freeEnv :: Env -> Set Name
freeEnv (Env env) = let allPTypes = M.elems env
                    in S.unions (map freePType allPTypes)



-- | Performing a 'Subst'itution in an 'Env'ironment means performing that
-- substituion on all the contained 'PType's.
substEnv :: Subst -> Env -> Env
substEnv s (Env env) = Env (M.map (substPType s) env)





-- #############################################################################
-- ** Substitutions
-- #############################################################################



-- | A substitution is a mapping from type variables to 'MType's. Applying a
-- substitution means applying those replacements.
--
-- If you want to unify @Int -> Int@ with @a -> a@, after inferring that
-- @a@ should be @Int@, that substituion has to be performed on the latter
-- value to get rid of all the @a@s.
newtype Subst = Subst (Map Name MType)

-- | The empty substituion holds nothing, and is the identity of 'compose'.
empty :: Subst
empty = Subst M.empty

-- | Combine two substitutions by merging them.
--
-- In addition to that, the first argument might contain substitutions
-- affecting the types mentioned in the right hand side argument, so in
-- addition to the merge, the first substitution is also applied to all
-- 'MType's mentioned by the second one.
compose :: Subst -> Subst -> Subst
compose subst1@(Subst s1) (Subst s2) = Subst (s1 `M.union` s2')
    where s2' = fmap (substMType subst1) s2





-- #############################################################################
-- #############################################################################
-- * Typechecking
-- #############################################################################
-- #############################################################################

-- $ Typechecking does two things:
--
-- 1. If two types are not immediately identical, attempt to 'unify' them
--    to get a type compatible with both of them
-- 2. 'infer' the most general type of a value by comparing the values in its
--    definition with the 'Env'ironment





-- #############################################################################
-- * Inference context
-- #############################################################################



-- | The inference monad. It holds a supply of unique names, and can fail with
-- a descriptive error if something goes wrong.
newtype Infer a = Infer { runInfer :: ExceptT Text (State Integer) a }
    deriving (Functor, Applicative, Monad)



-- | Generic function to fail with an error message.
throw :: Text -> Infer a
throw err = Infer (ExceptT (StateT (\s -> Identity (Left err, s))))





-- #############################################################################
-- ** Unification
-- #############################################################################

-- $ Unification describes the process of making two different types compatible
-- by specializing them where needed.


-- | The unification of two 'MType's is the most general substituion that can
-- be applied to both of them in order to yield the same result.
--
-- For example, trying to unify @a -> b@ with @c -> (Int -> Bool)@ will result
-- in a substitution of @a@ for @c@, and @b@ for @Int -> Bool@.
unify :: MType -> MType -> Infer Subst
unify (TFun a b) (TFun x y) = do
    subst1 <- unify a x
    subst2 <- unify (substMType subst1 b) (substMType subst1 y)
    pure (compose subst1 subst2)
unify (TVar v) x = bind v x
unify x (TVar v) = bind v x
unify a b = cannotUnify a b



-- | Build a 'Subst'itution that binds a 'Name' to an 'MType'.
--
-- This is what happens when a type variable is unified with another 'MType':
-- the variable is simply bound to that 'MType'.
bind :: Name -> MType -> Infer Subst
bind name (TVar v) | name == v = pure empty
bind name mType | name `S.member` freeMType mType = occursCheck name mType
bind name mType = pure (Subst (M.singleton name mType))



-- | Error if two types that don't match are unified.
cannotUnify :: MType -> MType -> Infer a
cannotUnify a b = throw ("Cannot unify " <> ppr a <> " and " <> ppr b)



-- | Error if the value of a type is specialized although another (incompatible)
-- specialization is already known.
occursCheck :: Name -> MType -> Infer a
occursCheck (Name n) a =
    throw ("Occurs check failed for " <> n <> " in " <> ppr a)





-- #############################################################################
-- ** Type inference
-- #############################################################################

-- $ Type inference is the act of finding out a value's type by looking at the
-- environment it is in, in order to make it compatible with it.
--
-- In literature, the Hindley-Damas-Milner inference algorithm ("algorithm W")
-- is often presented in the style of logical formulas, and below you'll find
-- that version along with code that actually does what they say.
--
-- These formulas look a bit like fractions, where the "numerator" is a
-- collection of premises, and the denominator is the consequence if all of them
-- hold.



-- | The syntax tree of the language we'd like to typecheck. You can view it
-- as a close relative to simply typed lambda calculus, having only the most
-- necessary syntax elements.
--
-- For example, the term @let y = λx. f x in z@ would be represented by
--
-- @
-- ELet "y"
--      (EAbs "x"
--            (EApp (EVar "f")
--                  (EVar "x")))
--      (EVar z)
-- @

data Exp = EVar Name         -- ^ @x@
         | EApp Exp Exp      -- ^ @f x@
         | EAbs Name Exp     -- ^ @\x -> e@
         | ELet Name Exp Exp -- ^ @let x = e in e'@



-- | Generate a fresh 'Name' in a type 'Infer'ence context. An example use case
-- of this is η expansion, which transforms @f@ into @λx. f x@, where "x" is a
-- new name.
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
-- gen(Γ,τ) = ∀{α}. σ
--     where {α} = free(τ) – free(Γ)
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
-- You can picture the 'PType' to be the prototype converted to an instantiated
-- 'MType', which can now be used in the unification process.
instantiate :: PType -> Infer MType
instantiate (Forall as t) = do
    subst <- fmap Subst (sequenceA (M.fromSet (const fresh) as))
    pure (substMType subst t)



-- | Infer the type of an 'Exp' in an 'Env', resulting in the 'Exp's 'MType'
-- along with a substitution that has to be done in order to reach this goal.
--
-- This is widely known as /Algorithm W/.
infer :: Env -> Exp -> Infer (Subst, MType)
infer env = \case
    EVar name   -> inferVar env name
    EApp f x    -> inferApp env f x
    EAbs x e    -> inferAbs env x e
    ELet x e e' -> inferLet env x e e'



-- | Inferring the type of a variable is done via
--
-- @
-- x:σ ∈ Γ   τ = inst(σ)
-- --------------------- [Var]
--      Γ ⊢ x:τ
-- @
--
-- which simply means that if @x@ is available in polymorphic form, then we have
-- it available in all possible instantiations of that σ type.
inferVar :: Env -> Name -> Infer (Subst, MType)
inferVar env name = do
    sigma <- lookupEnv env name -- x:σ ∈ Γ
    tau <- instantiate sigma    -- τ = inst(σ)
                                -- -----------
    pure (empty, tau)           -- Γ ⊢ x:τ



-- | Function application captures the fact that if we have a function and an
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
inferApp :: Env -> Exp -> Exp -> Infer (Subst, MType)
inferApp env f x = do
    (s1, fTau) <- infer env f                          -- f : fτ
    (s2, xTau) <- infer (substEnv s1 env) x            -- x : xτ
    fxTau <- fresh                                     -- fxτ = fresh
    s3 <- unify (substMType s2 fTau) (TFun xTau fxTau) -- unify'(fτ, xτ → fxτ)
    let s = s3 `compose` s2 `compose` s1               -- --------------------
    pure (s, substMType s3 fxTau)                      -- f x : fxτ



-- | Lambda abstraction is based on the fact that when we introduce a new
-- variable, the resulting lambda maps from that variable's type to the type
-- of the body.
--
-- The typing rule is
--
-- @
-- τ = fresh   σ = gen(Γ,τ)   Γ, x:σ ⊢ e:τ'
-- ---------------------------------------- [Abs]
--              Γ ⊢ λx.e : τ→τ'
-- @
--
-- Here, @Γ, x:τ@ is @Γ@ extended by one additional mapping, namely @x:τ@.
inferAbs :: Env -> Name -> Exp -> Infer (Subst, MType)
inferAbs env x e = do
    tau <- fresh                           -- τ = fresh
    let sigma = generalize env tau         -- σ = gen(Γ, τ)
        env' = extendEnv env x sigma       -- Γ, x:σ …
    (s, tau') <- infer env' e              --        … ⊢ e:τ'
                                           -- ---------------
    pure (s, TFun (substMType s tau) tau') -- λx.e : τ→τ'



-- | A let binding allows extending the environment with new bindings in a
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
inferLet :: Env -> Name -> Exp -> Exp -> Infer (Subst, MType)
inferLet env x e e' = do
    (s1, tau) <- infer env e           -- Γ ⊢ e:τ
    let env' = substEnv s1 env
    let sigma = generalize env' tau    -- σ = gen(Γ,τ)
    let env'' = extendEnv env' x sigma -- Γ, x:σ
    (s2, tau') <- infer env'' e'       -- Γ ⊢ …
                                       -- --------------------------
    pure (s2 `compose` s1, tau')       --     … let x = e in e' : τ'














































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


--   Subst s1 <> (Subst s2 <> Subst s3)
-- = Subst s1 <> Subst (s2 `M.union` fmap (f s2) s3)
-- = Subst (s1 `M.union` fmap (f s1) (s2 `M.union` fmap (f s2) s3))
-- = Subst (s1 `M.union` (fmap (f s1) s2 `M.union` fmap (f s1) (fmap (f s2) s3)))
-- = Subst (s1 `M.union` (fmap (f s1) s2 `M.union` fmap (f s1 . f s2) s3))
-- = Subst (s1 `M.union` fmap (f s1) s2 `M.union` fmap (f s1 . f s2) s3)

--   (Subst s1 <> Subst s2) <> Subst s3
-- = Subst (s1 `M.union` fmap (f s1) s2) <> Subst s3
-- = Subst ((s1 `M.union` fmap (f s1) s2) `M.union` fmap (f (s1 `M.union` fmap (f s1) s2)) s3)
-- = Subst (s1 `M.union` fmap (f s1) s2 `M.union` fmap (f (s1 `M.union` fmap (f s1) s2)) s3)


--------------------------------------------------------------------------------




--------------------------------------------------------------------------------



--------------------------------------------------------------------------------