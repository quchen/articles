{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}



-- | __WORK IN PROGRESS.__ Errors expected, please don't post it anywhere.
--
--
-- This module is an extensively documented walkthrough for typechecking a
-- basic functional language using the Hindley-Damas-Milner algorithm.
--
-- It can be used in three different forms:
--
-- - The source is written in literate programming style, so you can almost
--   read it from top to bottom, minus some few references to later.
-- - Runnable in GHCi.
-- - The Haddock output yields a nice overview over the definitions given.
--   It's not as good of a read as the source since many of the important
--   inter-code comments are not visible.

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
import qualified Data.Text.IO               as T






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





-- #############################################################################
-- ** Names
-- #############################################################################



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
data MType = TVar Name           -- ^ @a@
           | TFun MType MType    -- ^ @a -> b@
           | TList MType         -- ^ @[a]@
           | TEither MType MType -- ^ @Either a b@
           | TTuple MType MType  -- ^ @(a,b)@
           | TConst Name         -- ^ @Int@, @()@, ...

-- | @
-- TFun (TVar "a") (TVar "b")
-- >>> a → b
-- @
instance Pretty MType where
    ppr = go False
      where
        go _ (TVar name)   = ppr name
        go _ (TList a)     = "[" <> ppr a <> "]"
        go _ (TEither l r) = "Either " <> ppr l <> " " <> ppr r
        go _ (TTuple a b)  = "(" <> ppr a <> ", " <> ppr b <> ")"
        go _ (TConst name) = ppr name
        go parenthesize (TFun a b)
            | parenthesize = "(" <> lhs <> " → " <> rhs <> ")"
            | otherwise    =        lhs <> " → " <> rhs
            where lhs = go True a
                  rhs = go False b

-- | 'String' → 'TVar'
instance IsString MType where
    fromString = TVar . fromString



-- | The free variables of an 'MType'. This is simply the collection of all the
-- individual type variables occurring inside of it.
--
-- __Example:__ The free variables of @a -> b@ are @a@ and @b@.
freeMType :: MType -> Set Name
freeMType = \case
    TVar a      -> [a]
    TFun f x    -> freeMType f <> freeMType x
    TList a     -> freeMType a
    TEither l r -> freeMType l <> freeMType r
    TTuple a b  -> freeMType a <> freeMType b
    TConst _    -> []




-- | Apply a substitution to all known variables contained in an 'MType'.
-- Variables not mentioned are left unchanged.
--
-- This operation will not change the 'MType' constructors that may be contained
-- inside the 'MType' recursively, it only acts on type variables.
substMType :: Subst -> MType -> MType
substMType s = \case
    TVar a -> let Subst s' = s
              in M.findWithDefault (TVar a) a s'
    TFun f x      -> TFun (rec f) (rec x)
    TList a       -> TList (rec a)
    TEither l r   -> TEither (rec l) (rec r)
    TTuple a b    -> TTuple (rec a) (rec b)
    c@(TConst {}) -> c
  where
    rec = substMType s





-- #############################################################################
-- ** Polytypes
-- #############################################################################


-- | A polytype is a monotype universally quantified over a number of type
-- variables. In Haskell, all definitions have polytypes, but since the @forall@
-- is implicit they look a bit like monotypes, maybe confusingly so.
--
-- A polytype claims to work "for all imaginable type parameters", very similar
-- to how a lambda claims to work "for all imaginable value parameters". We can
-- insert a value into a lambda's parameter to evaluate it to a new value, and
-- similarly we'll later insert types into a polytype's quantified variables
-- to gain new types.
--
-- __Example:__ in a definition @id :: forall a. a -> a@, the @a@ after the
-- ∀ ("forall") is the collection of type variables, and @a -> a@ is the
-- 'MType' quantified over. When we have such an @id@, we also have its
-- specialized version @Int -> Int@ available. This process will be the
-- topic of the type inference/unification algorithms.
--
-- In formal notation, 'PType's are often called σ (sigma) types.
data PType = Forall (Set Name) MType

-- | @
-- Forall ["a"] (TFun "a" "a")
-- >>> ∀a. a → a
-- @
instance Pretty PType where
    ppr (Forall qs mType) = "∀" <> universals <> ". " <> ppr mType
      where
        universals = T.intercalate " " (map ppr (S.toList qs))



-- | The free variables of a 'PType' are the free variables of the contained
-- 'MType', except those universally quantified.
--
-- __Example:__ @foo :: forall a. a -> b -> a@ would be a 'PType' in which
-- @b@ is a free type variable, while @a@ is bound (via the @forall@).
freePType :: PType -> Set Name
freePType (Forall qs mType) = freeMType mType `S.difference` qs



-- | Apply a substitution to a 'PType', replacing all known variables in the
-- contained 'MType' except the ones universally quantified.
--
-- Invariant: the quantified variables are not changed by the operation.
substPType :: Subst -> PType -> PType
substPType (Subst subst) (Forall qs mType) =
    let qs' = M.fromSet (const ()) qs
        subst' = Subst (subst `M.difference` qs')
    in Forall qs (substMType subst' mType)





-- #############################################################################
-- ** The environment
-- #############################################################################



-- | The environment consists of all the values available in scope, and their
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
    ppr (Env env) = "Γ = \n" <> T.intercalate "\n" pprBindings
      where
        bindings = M.assocs env
        pprBinding (name, pType) = "  " <> ppr name <> " ≡ " <> ppr pType
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
--
-- A maybe more intuitive way that will come in handy for 'compose' is to view
-- a 'Subst'itution as a chain of
newtype Subst = Subst (Map Name MType)



-- | a ⇒ b, c ⇒ d → e
instance Pretty Subst where
    ppr (Subst s) = T.intercalate ", " $ [ ppr k <> " ⇒ " <> ppr v | (k,v) <- M.toList s ]



-- | The empty substituion holds nothing, and is the identity of 'compose'.
empty :: Subst
empty = Subst M.empty



-- | Combine two substitutions. Applying the resulting substitution means
-- applying the right hand side substitution first, and then the left hand
-- side.
--
-- In the implementation of 'compose', we don't apply the substitution to
-- anything of course, so we have to incorporate the first (higher priority)
-- substitution into the second one beforehand, done via 'substSubst'.
compose :: Subst -> Subst -> Subst
compose subst1 subst2 = Subst (s1 `M.union` s2)
  where
    Subst s1 = subst1
    Subst s2 = substSubst subst1 subst2

    -- Apply one substitution to another, replacing all the bindings in the
    -- second argument with their values mentioned in the first one.
    substSubst :: Subst -- Apply this ...
               -> Subst -- ... to this
               -> Subst
    substSubst s (Subst target) = Subst (fmap (substMType s) target)


-- I have a strong feeling that 'compose' is associative, which would make
-- 'Subst' a 'Monoid', but I wasn't able to prove this yet.
--
-- instance Monoid Subst where
--     mappend = compose
--     mempty = empty





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



-- | The inference type holds a supply of unique names, and can fail with
-- a descriptive error if something goes wrong.
newtype Infer a = Infer (ExceptT InferError (State [Text]) a)
    deriving (Functor, Applicative, Monad)


data InferError =
    -- | Two types that don't match were attempted to be unified.
      CannotUnify MType MType

    -- | A 'TVar' is bound to an 'MType' that already contains it.
    --
    -- The canonical example of this is @λx. x x@, where the first @x@
    -- in the body has to have type @a → b@, and the second one @a@. Since
    -- they're both the same @x@, this requires unification of @a@ with @a → b@,
    -- which only works if @a = a → b = (a → b) → b = ...@, yielding an infinite
    -- type.
    | OccursCheckFailed Name MType

    -- | The value of an unknown identifier was read.
    | UnknownIdentifier Name

    -- | The supply of 'fresh' variable names has run out.
    | OutOfFreshNames

instance Pretty InferError where
    ppr (CannotUnify t1 t2) = "Cannot unify " <> ppr t1 <> " with " <> ppr t2
    ppr (OccursCheckFailed name ty) = "Occurs check failed: " <> ppr name <> " already appears in " <> ppr ty
    ppr (UnknownIdentifier name) = "Unknown identifier: " <> ppr name
    ppr OutOfFreshNames = "Fresh type variable name supply empty"



-- | Evaluate a value in an 'Infer'ence context.
runInfer :: [Text] -- ^ Supply of variable names.
         -> Infer a -- ^ Inference data
         -> Either InferError a
runInfer supply (Infer inf) =
    runIdentity (evalStateT (runExceptT inf) infiniteSupply)
  where
    -- [a, b, c] ==> [a,b,c, a1,b1,c1, a2,b2,c2, ...]
    infiniteSupply = supply <> addSuffixes supply (1 :: Integer)
    addSuffixes xs n = zipWith addSuffix xs (repeat n) <> addSuffixes xs (n+1)
    addSuffix x n = x <> T.pack (show n)



-- | Throw an 'InferError' in an 'Infer'ence context.
throw :: InferError -> Infer a
throw err = Infer (ExceptT (StateT (\s -> Identity (Left err, s))))





-- #############################################################################
-- ** Unification
-- #############################################################################

-- $ Unification describes the process of making two different types compatible
-- by specializing them where needed.


-- | The unification of two 'MType's is the most general substituion that can
-- be applied to both of them in order to yield the same result.
--
-- __Example:__ trying to unify @a -> b@ with @c -> (Int -> Bool)@ will result
-- in a substitution of @a@ for @c@, and @b@ for @Int -> Bool@.
unify :: (MType, MType) -> Infer Subst
unify = \case
    (TFun a b,    TFun x y)          -> unifyBinary (a,b) (x,y)
    (TVar v,      x)                 -> v `bindVariableTo` x
    (x,           TVar v)            -> v `bindVariableTo` x
    (TConst a,    TConst b) | a == b -> pure empty
    (TList a,     TList b)           -> unify (a,b)
    (TEither a b, TEither x y)       -> unifyBinary (a,b) (x,y)
    (TTuple a b,  TTuple x y)        -> unifyBinary (a,b) (x,y)
    (a, b)                           -> throw (CannotUnify a b)

  where

    -- Unification of binary type constructors, such as functions and Either.
    -- Unification is first done for the first operand, and assuming the
    -- required substitution, the second operands are unified.
    unifyBinary (a,b) (x,y) = do
        s1 <- unify (a, x)
        let b' = substMType s1 b
            y' = substMType s1 y
        s2 <- unify (b', y')
        pure (s1 `compose` s2)



-- | Build a 'Subst'itution that binds a 'Name' of a 'TVar' to an 'MType'.
-- The resulting substitution should be idempotent, i.e. applying it more than
-- once to something should not be any different from applying it only once.
--
-- - In the simplest case, this just means building a substitution that just
--   does that.
-- - Substituting a 'Name' with a 'TVar' with the same name unifies a type
--   variable with itself, and the resulting substitution does nothing new.
-- - If the 'Name' we're trying to bind to an 'MType' already occurs in that
--   'MType', the resulting substitution would not be idempotent: the 'MType'
--   would be replaced again, yielding a different result.
bindVariableTo :: Name -> MType -> Infer Subst
bindVariableTo name (TVar v) | boundToSelf = pure empty
  where boundToSelf = name == v
bindVariableTo name mType | bindingIsRecursive = throw (OccursCheckFailed name mType)
  where bindingIsRecursive = name `S.member` freeMType mType
bindVariableTo name mType = pure (Subst (M.singleton name mType))






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






-- -----------------------------------------------------------------------------
-- *** The language: typed lambda calculus
-- -----------------------------------------------------------------------------



-- | The syntax tree of the language we'd like to typecheck. You can view it
-- as a close relative to simply typed lambda calculus, having only the most
-- necessary syntax elements.
--
-- __Example:__ the term @let y = λx. f x in z@ would be represented by
--
-- @
-- ELet "y"
--      (EAbs "x"
--            (EApp (EVar "f")
--                  (EVar "x")))
--      (EVar z)
-- @
data Exp = ELit Lit          -- ^ True, 1
         | EVar Name         -- ^ @x@
         | EApp Exp Exp      -- ^ @f x@
         | EAbs Name Exp     -- ^ @\x -> e@
         | ELet Name Exp Exp -- ^ @let x = e in e'@



-- | Literals we'd like to support. Since we can't define new data types in our
-- simple type system, we'll have to hard-code the possible ones here.
data Lit = LBool Bool
         | LInteger Integer



-- | Omit redundant parentheses, group chained lambdas.
--
-- @
-- EAbs "f"
--   (EAbs "g"
--     (EAbs "x"
--       (EApp (EApp "f" "x")
--             (EApp "g" "x"))))
-- >>> λf g x. f x (g x)
-- @
instance Pretty Exp where
    ppr (ELit lit) = ppr lit

    ppr (EVar name) = ppr name

    ppr (EApp f x) = pprApp1 f <> " " <> pprApp2 x
      where
        pprApp1 = \case
            eLet@(ELet {}) -> "(" <> ppr eLet <> ")"
            eLet@(EAbs {}) -> "(" <> ppr eLet <> ")"
            e -> ppr e
        pprApp2 = \case
            eApp@(EApp {}) -> "(" <> ppr eApp <> ")"
            e -> pprApp1 e

    ppr x@(EAbs {}) = pprAbs True x
      where
        pprAbs True  (EAbs name expr) = "λ" <> ppr name <> pprAbs False expr
        pprAbs False (EAbs name expr) = " " <> ppr name <> pprAbs False expr
        pprAbs _ expr = ". " <> ppr expr

    ppr (ELet name value body) =
        "let " <> ppr name <> " = " <> ppr value <> " in " <> ppr body

instance Pretty Lit where
    ppr = \case
        LBool    b -> showT b
        LInteger i -> showT i
      where
        showT :: Show a => a -> Text
        showT = T.pack . show

-- | 'String' → 'EVar'
instance IsString Exp where
    fromString = EVar . fromString





-- -----------------------------------------------------------------------------
-- *** Some useful definitions
-- -----------------------------------------------------------------------------



-- | Generate a fresh 'Name' in a type 'Infer'ence context. An example use case
-- of this is η expansion, which transforms @f@ into @λx. f x@, where "x" is a
-- new name, i.e. unbound in the current context.
fresh :: Infer MType
fresh = drawFromSupply >>= \case
    Right name -> pure (TVar name)
    Left err -> throw err
  where
    drawFromSupply :: Infer (Either InferError Name)
    drawFromSupply = Infer (
        lift get >>= \case
            s:upply -> do lift (put upply)
                          pure (Right (Name s))
            _ -> pure (Left OutOfFreshNames) )



-- | Lift a 'fresh'ly generated 'MType' into a 'PType' by quantifying over no
-- variables. This is only safe to use if no variable duplication can be
-- ensured, as is the case when the argument is 'fresh'.
--
-- @
-- a → b  ⇒  ∀∅ a → b
-- @
liftFresh :: MType -> PType
liftFresh = Forall []



-- | Add a new binding to the environment.
--
-- The Haskell equivalent would be defining a new value, for example in module
-- scope or in a @let@ block.
--
-- @
-- Γ, x:σ  ≡  extendEnv Γ (x,σ)
-- @
extendEnv :: Env -> (Name, PType) -> Env
extendEnv (Env env) (name, pType) = Env (M.insert name pType env)





-- -----------------------------------------------------------------------------
-- *** Inferring the types of all language constructs
-- -----------------------------------------------------------------------------



-- | Infer the type of an 'Exp'ression in an 'Env'ironment, resulting in the
-- 'Exp's 'MType'along with a substitution that has to be done in order to
-- reach this goal.
--
-- This is widely known as /Algorithm W/.
infer :: Env -> Exp -> Infer (Subst, MType)
infer env = \case
    ELit lit    -> inferLit lit
    EVar name   -> inferVar env name
    EApp f x    -> inferApp env f x
    EAbs x e    -> inferAbs env x e
    ELet x e e' -> inferLet env x e e'



-- | Literals such as 'True' and '1' have their types hard-coded.
inferLit :: Lit -> Infer (Subst, MType)
inferLit lit = pure (empty, TConst litTy)
  where
    litTy = case lit of
        LBool    {} -> "Bool"
        LInteger {} -> "Integer"



-- | Look up the 'PType' of a 'Name' in the 'Env'ironment.
--
-- To give a Haskell analogon, looking up @id@ when @Prelude@ is loaded,
-- the resulting 'PType' would be @id@'s type, namely @forall a. a -> a@.
lookupEnv :: Env -> Name -> Infer PType
lookupEnv (Env env) name = case M.lookup name env of
    Just x -> pure x
    Nothing -> throw (UnknownIdentifier name)



-- | Bind all quantified variables of a 'PType' to 'fresh' type variables.
--
-- __Example:__ instantiating @forall a. a -> b -> a@ results in the 'MType'
-- @a -> b -> a@, where @a@ is a fresh name (to avoid shadowing issues).
--
-- You can picture the 'PType' to be the prototype converted to an instantiated
-- 'MType', which can now be used in the unification process.
--
-- Another way of looking at it is by simply forgetting which variables were
-- quantified over, carefully avoiding name clashes when doing so.
instantiate :: PType -> Infer MType
instantiate (Forall qs t) = do
    subst <- substituteAllWithFresh qs
    pure (substMType subst t)

  where
    -- For each given name, add a substitution from that name to a fresh type
    -- variable to the result.
    substituteAllWithFresh :: Set Name -> Infer Subst
    substituteAllWithFresh xs = do
        let freshSubstActions = M.fromSet (const fresh) xs
        freshSubsts <- sequenceA freshSubstActions
        pure (Subst freshSubsts)



-- | Inferring the type of a variable is done via
--
-- @
-- x:σ ∈ Γ   τ = instantiate(σ)
-- ----------------------------  [Var]
--            Γ ⊢ x:τ
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
-- argument we can give to that function, we also have the result value of the
-- result type available to us.
--
-- @
-- Γ ⊢ f : fτ   Γ ⊢ x : xτ   fxτ = fresh   unify(fτ, xτ → fxτ)
-- -----------------------------------------------------------  [App]
--                       Γ ⊢ f x : fxτ
-- @
--
-- This rule is however a bit backwards to our normal way of thinking about
-- typing function application: instead of applying a function to a value
-- and investigating the result, we hypothesize the function type @xτ → fxτ@
-- of mappting the argument @xτ@ to the result type @fxτ@, and make sure this
-- mapping unifies with the function @f:fτ@ given.
inferApp :: Env -> Exp -> Exp -> Infer (Subst, MType)
inferApp env f x = do
    (s1, fTau) <- infer env f                         -- f : fτ
    (s2, xTau) <- infer (substEnv s1 env) x           -- x : xτ
    fxTau <- fresh                                    -- fxτ = fresh
    s3 <- unify (substMType s2 fTau, TFun xTau fxTau) -- unify (fτ, xτ → fxτ)
    let s = s3 `compose` s2 `compose` s1              -- --------------------
    pure (s, substMType s3 fxTau)                     -- f x : fxτ



-- | Lambda abstraction is based on the fact that when we introduce a new
-- variable, the resulting lambda maps from that variable's type to the type
-- of the body.
--
-- @
-- τ = fresh   σ = liftFresh(τ)   Γ, x:σ ⊢ e:τ'
-- --------------------------------------------  [Abs]
--                Γ ⊢ λx.e : τ→τ'
-- @
--
-- Here, @Γ, x:τ@ is @Γ@ extended by one additional mapping, namely @x:τ@.
--
-- Abstraction is typed by extending the environment by a new 'MType', and if
-- under this assumption we can construct a function mapping to a value of
-- that type, we can say that the lambda takes a value and maps to it.
inferAbs :: Env -> Name -> Exp -> Infer (Subst, MType)
inferAbs env x e = do
    tau <- fresh                           -- τ = fresh
    let sigma = liftFresh tau              -- σ = liftFresh τ
        env' = extendEnv env (x, sigma)    -- Γ, x:σ …
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
-- ---------------------------------------  [Let]
--         Γ ⊢ let x = e in e' : τ'
-- @
inferLet :: Env -> Name -> Exp -> Exp -> Infer (Subst, MType)
inferLet env x e e' = do
    (s1, tau) <- infer env e              -- Γ ⊢ e:τ
    let env' = substEnv s1 env
    let sigma = generalize env' tau       -- σ = gen(Γ,τ)
    let env'' = extendEnv env' (x, sigma) -- Γ, x:σ
    (s2, tau') <- infer env'' e'          -- Γ ⊢ …
                                          -- --------------------------
    pure (s2 `compose` s1, tau')          --     … let x = e in e' : τ'



-- | Generalize an 'MType' to a 'PType' by universally quantifying over
-- all the type variables contained in it, except those already mentioned
-- in the environment.
--
-- __Example:__ Generalizing @forall a. a -> b -> a@ yields
-- @forall a b. a -> b -> a@.
--
-- @
-- gen(Γ,τ) = ∀{α}. σ
--     where {α} = free(τ) – free(Γ)
-- @
generalize :: Env -> MType -> PType
generalize env mType = Forall qs mType
  where
    qs = freeMType mType `S.difference` freeEnv env





-- #############################################################################
-- #############################################################################
-- * Testing
-- #############################################################################
-- #############################################################################



-- #############################################################################
-- ** Convenience functions
-- #############################################################################



-- | Synonym for 'TFun' to make writing type signatures easier.
--
-- Instead of
--
-- @
-- Forall ["a","b"] (TFun "a" (TFun "b" "a"))
-- @
--
-- we can write
--
-- @
-- Forall ["a","b"] ("a" ~> "b" ~> "a")
-- @
(~>) :: MType -> MType -> MType
(~>) = TFun
infixr 9 ~>

-- | Build multiple lambda bindings.
--
-- Instead of
--
-- @
-- EAbs "f" (EAbs "x" (EApp "f" "x"))
-- @
--
-- we can write
--
-- @
-- lambda ["f", "x"] (EApp "f" "x")
-- @
--
-- for
--
-- @
-- λf x. f x
-- @
lambda :: [Name] -> Exp -> Exp
lambda names expr = foldr EAbs expr names



-- | Apply a function to multiple arguments.
--
-- Instead of
--
-- @
-- EApp (EApp (EApp "f" "x") "y") "z")
-- @
--
-- we can write
--
-- @
-- apply "f" ["x", "y", "z"]
-- @
--
-- for
--
-- @
-- f x y z
-- @
apply :: Exp -> [Exp] -> Exp
apply = foldl EApp



-- | Construct an integer literal.
int :: Integer -> Exp
int = ELit . LInteger



-- | Construct a boolean literal.
bool :: Bool -> Exp
bool = ELit . LBool





-- #############################################################################
-- ** The environment
-- #############################################################################



prelude :: Env
prelude = Env (M.fromList
    [ ("(*)",        Forall []              (tInteger ~> tInteger ~> tInteger))
    , ("(+)",        Forall []              (tInteger ~> tInteger ~> tInteger))
    , ("(,)",        Forall ["a","b"]       ("a" ~> "b" ~> TTuple "a" "b"))
    , ("(-)",        Forall []              (tInteger ~> tInteger ~> tInteger))
    , ("(.)",        Forall ["a", "b", "c"] (("b" ~> "c") ~> ("a" ~> "b") ~> "a" ~> "c"))
    , ("(<)",        Forall []              (tInteger ~> tInteger ~> tBool))
    , ("(<=)",       Forall []              (tInteger ~> tInteger ~> tBool))
    , ("(>)",        Forall []              (tInteger ~> tInteger ~> tBool))
    , ("(>=)",       Forall []              (tInteger ~> tInteger ~> tBool))
    , ("const",      Forall ["a","b"]       ("a" ~> "b" ~> "a"))
    , ("Cont/>>=",   Forall ["a"]           ((("a" ~> "r") ~> "r") ~> ("a" ~> (("b" ~> "r") ~> "r")) ~> (("b" ~> "r") ~> "r")))
    , ("find",       Forall ["a","b"]       (("a" ~> tBool) ~> TList "a" ~> tMaybe "a"))
    , ("fix",        Forall ["a"]           (("a" ~> "a") ~> "a"))
    , ("foldr",      Forall ["a","b"]       (("a" ~> "b" ~> "b") ~> "b" ~> TList "a" ~> "b"))
    , ("id",         Forall ["a"]           ("a" ~> "a"))
    , ("ifThenElse", Forall ["a"]           (tBool ~> "a" ~> "a" ~> "a"))
    , ("Left",       Forall ["a","b"]       ("a" ~> TEither "a" "b"))
    , ("length",     Forall ["a"]           (TList "a" ~> tInteger))
    , ("map",        Forall ["a","b"]       (("a" ~> "b") ~> TList "a" ~> TList "b"))
    , ("reverse",    Forall ["a"]           (TList "a" ~> TList "a"))
    , ("Right",      Forall ["a","b"]       ("b" ~> TEither "a" "b"))
    ])
  where
    tBool = TConst "Bool"
    tInteger = TConst "Integer"
    tMaybe = TEither (TConst "()")



-- | Supply to draw fresh type variable names from.
defaultSupply :: [Text]
defaultSupply = map (T.pack . pure) ['a'..'z']





-- #############################################################################
-- ** Run it!
-- #############################################################################



-- | Convenience function to run type inference algorithm
showType :: Env    -- ^ Starting environment, e.g. 'prelude'.
         -> [Text] -- ^ Fresh variable name supply. Should be non-empty.
         -> Exp    -- ^ Expression to typecheck
         -> Text   -- ^ Text representation of the result. Contains an error
                   --   message on failure.
showType env supply expr =
    case (runInfer supply . fmap snd . infer env) expr of
        Left err -> "Error inferring type of " <> ppr expr <>": " <> ppr err
        Right ty -> ppr expr <> " :: " <> ppr ty



-- | Run type inference on a cuple of values
main :: IO ()
main = do
    let run = T.putStrLn . ("  " <>) . showType prelude defaultSupply
    T.putStrLn "Well-typed:"
    run (lambda ["x"] "x")
    run (lambda ["f","g","x"] (apply "f" ["x", apply "g" ["x"]]))
    run (lambda ["f","g","x"] (apply "f" [apply "g" ["x"]]))
    run (apply "find" [lambda ["x"] (apply "(>)" ["x", int 0])])
    run (lambda ["f"] (apply "(.)" ["reverse", apply "map" ["f"]]))
    run (apply "map" [apply "map" ["map"]])
    run (apply "(*)" [int 1, int 2])
    run (apply "foldr" ["(+)", int 0])
    run (apply "map" ["length"])
    run (apply "map" ["map"])
    run (lambda ["x"] (apply "ifThenElse" [apply "(<)" ["x", int 0], int 0, "x"]))
    T.putStrLn "Ill-typed:"
    run (apply "(*)" [int 1, bool True])
    run (apply "foldr" [int 1])
    run (lambda ["x"] (apply "x" ["x"]))
