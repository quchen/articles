{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}



-- | This module is an extensively documented walkthrough for typechecking a
-- basic functional language using the Hindley-Damas-Milner algorithm.
--
-- In the end, we'll be able to infer the type of expressions like
--
-- @
-- find (λx. (>) x 0)
--   :: [Integer] -> Either () Integer
-- @
--
-- It can be used in multiple different forms:
--
--  * The source is written in literate programming style, so you can almost
--    read it from top to bottom, minus some few references to later topics.
--  * /Loads/ of doctests (runnable and verified code examples) are included
--  * The code is runnable in GHCi, all definitions are exposed.
--  * A small main module that gives many examples of what you might try out in
--    GHCi is also included.
--  * The Haddock output yields a nice overview over the definitions given, with
--    a nice rendering of a truckload of Haddock comments.

module HindleyMilner where

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



-- $setup
--
-- For running doctests:
--
-- >>> :set -XOverloadedStrings
-- >>> :set -XOverloadedLists
-- >>> :set -XLambdaCase
-- >>> import qualified Data.Text.IO as T
-- >>> let putPprLn = T.putStrLn . ppr



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
    deriving (Eq, Ord, Show)

-- | >>> "lorem" :: Name
-- Name "lorem"
instance IsString Name where
    fromString = Name . T.pack

-- | >>> putPprLn (Name "var")
-- var
instance Pretty Name where
    ppr (Name n) = n



-- #############################################################################
-- ** Monotypes
-- #############################################################################



-- | A monotype is an unquantified/unparametric type, in other words it contains
-- no @forall@s. Monotypes are the inner building blocks of all types. Examples
-- of monotypes are @Int@, @a@, @a -> b@.
--
-- In formal notation, 'MType's are often called τ (tau) types.
data MType = TVar Name           -- ^ @a@
           | TFun MType MType    -- ^ @a -> b@
           | TConst Name         -- ^ @Int@, @()@, …

           -- Since we can't declare our own types in our simple type system
           -- here, we'll hard-code certain basic ones so we can typecheck some
           -- familar functions that use them later.
           | TList MType         -- ^ @[a]@
           | TEither MType MType -- ^ @Either a b@
           | TTuple MType MType  -- ^ @(a,b)@
           deriving Show

-- | >>> putPprLn (TFun (TEither (TVar "a") (TVar "b")) (TFun (TVar "c") (TVar "d")))
-- Either a b → c → d
--
-- Using the 'IsString' instance:
--
-- >>> putPprLn (TFun (TEither "a" "b") (TFun "c" "d"))
-- Either a b → c → d
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

-- | >>> "var" :: MType
-- TVar (Name "var")
instance IsString MType where
    fromString = TVar . fromString



-- | The free variables of an 'MType'. This is simply the collection of all the
-- individual type variables occurring inside of it.
--
-- __Example:__ The free variables of @a -> b@ are @a@ and @b@.
freeMType :: MType -> Set Name
freeMType = \case
    TVar a      -> [a]
    TFun a b    -> freeMType a <> freeMType b
    TList a     -> freeMType a
    TEither l r -> freeMType l <> freeMType r
    TTuple a b  -> freeMType a <> freeMType b
    TConst _    -> []



-- | Substitute all the contained type variables mentioned in the substitution,
-- and leave everything else alone.
instance Substitutable MType where
    applySubst s = \case
        TVar a -> let Subst s' = s
                  in M.findWithDefault (TVar a) a s'
        TFun f x    -> TFun (applySubst s f) (applySubst s x)
        TList a     -> TList (applySubst s a)
        TEither l r -> TEither (applySubst s l) (applySubst s r)
        TTuple a b  -> TTuple (applySubst s a) (applySubst s b)
        c@TConst {} -> c



-- #############################################################################
-- ** Polytypes
-- #############################################################################

-- | A polytype is a monotype universally quantified over a number of type
-- variables. In Haskell, all definitions have polytypes, but since the @forall@
-- is implicit they look a bit like monotypes, maybe confusingly so. For
-- example, the type of @1 :: Int@ is actually @forall <nothing>. Int@, and the
-- type of @id@ is @forall a. a -> a@, although GHC displays it as @a -> a@.
--
-- A polytype claims to work "for all imaginable type parameters", very similar
-- to how a lambda claims to work "for all imaginable value parameters". We can
-- insert a value into a lambda's parameter to evaluate it to a new value, and
-- similarly we'll later insert types into a polytype's quantified variables to
-- gain new types.
--
-- __Example:__ in a definition @id :: forall a. a -> a@, the @a@ after the
-- ∀ ("forall") is the collection of type variables, and @a -> a@ is the 'MType'
-- quantified over. When we have such an @id@, we also have its specialized
-- version @Int -> Int@ available. This process will be the topic of the type
-- inference/unification algorithms.
--
-- In formal notation, 'PType's are often called σ (sigma) types.
--
-- The purpose of having monotypes and polytypes is that we'd like to only have
-- universal quantification at the top level, restricting our language to rank-1
-- polymorphism, where type inferece is total (all types can be inferred) and
-- simple (only a handful of typing rules). Weakening this constraint would be
-- easy: if we allowed universal quantification within function types we would
-- get rank-N polymorphism. Taking it even further to allow it anywhere,
-- effectively replacing all occurrences of 'MType' with 'PType', yields
-- impredicative types. Both these extensions make the type system
-- *significantly* more complex though.
data PType = Forall (Set Name) MType -- ^ ∀{α}. τ

-- | >>> putPprLn (Forall ["a"] (TFun "a" "a"))
-- ∀a. a → a
instance Pretty PType where
    ppr (Forall qs mType) = "∀" <> pprUniversals <> ". " <> ppr mType
      where
        pprUniversals
          | S.null qs = "∅"
          | otherwise = (T.intercalate " " . map ppr . S.toList) qs



-- | The free variables of a 'PType' are the free variables of the contained
-- 'MType', except those universally quantified.
--
-- >>> let sigma = Forall ["a"] (TFun "a" (TFun (TTuple "b" "a") "c"))
-- >>> putPprLn sigma
-- ∀a. a → (b, a) → c
-- >>> let display = T.putStrLn . T.intercalate ", " . foldMap (\x -> [ppr x])
-- >>> display (freePType sigma)
-- b, c
freePType :: PType -> Set Name
freePType (Forall qs mType) = freeMType mType `S.difference` qs



-- | Substitute all the free type variables.
instance Substitutable PType where
    applySubst (Subst subst) (Forall qs mType) =
        let qs' = M.fromSet (const ()) qs
            subst' = Subst (subst `M.difference` qs')
        in Forall qs (applySubst subst' mType)



-- #############################################################################
-- ** The environment
-- #############################################################################



-- | The environment consists of all the values available in scope, and their
-- associated polytypes. Other common names for it include "(typing) context",
-- and because of the commonly used symbol for it sometimes directly
-- \"Gamma"/@"Γ"@.
--
-- There are two kinds of membership in an environment,
--
--   - @∈@: an environment @Γ@ can be viewed as a set of @(value, type)@ pairs,
--     and we can test whether something is /literally contained/ by it via
--     x:σ ∈ Γ
--   - @⊢@, pronounced /entails/, describes all the things that are well-typed,
--     given an environment @Γ@. @Γ ⊢ x:τ@ can thus be seen as a judgement that
--     @x:τ@ is /figuratively contained/ in @Γ@.
--
-- For example, the environment @{x:Int}@ literally contains @x@, but given
-- this, it also entails @λy. x@, @λy z. x@, @let id = λy. y in id x@ and so on.
--
-- In Haskell terms, the environment consists of all the things you currently
-- have available, or that can be built by comining them. If you import the
-- Prelude, your environment entails
--
-- @
-- id            →  ∀a. a→a
-- map           →  ∀a b. (a→b) → [a] → [b]
-- putStrLn      →  ∀∅. String → IO ()
-- …
-- id map        →  ∀a b. (a→b) → [a] → [b]
-- map putStrLn  →  ∀∅. [String] -> [IO ()]
-- …
-- @
newtype Env = Env (Map Name PType)

-- | >>> :{
--    putPprLn (Env
--        [ ("id", Forall ["a"] (TFun "a" "a"))
--        , ("const", Forall ["a", "b"] (TFun "a" (TFun "b" "a"))) ])
-- :}
-- Γ = { const : ∀a b. a → b → a
--     , id : ∀a. a → a }
instance Pretty Env where
    ppr (Env env) = "Γ = { " <> T.intercalate "\n    , " pprBindings <> " }"
      where
        bindings = M.assocs env
        pprBinding (name, pType) = ppr name <> " : " <> ppr pType
        pprBindings = map pprBinding bindings



-- | The free variables of an 'Env'ironment are all the free variables of the
-- 'PType's it contains.
freeEnv :: Env -> Set Name
freeEnv (Env env) = let allPTypes = M.elems env
                    in S.unions (map freePType allPTypes)



-- | Performing a 'Subst'itution in an 'Env'ironment means performing that
-- substituion on all the contained 'PType's.
instance Substitutable Env where
    applySubst s (Env env) = Env (M.map (applySubst s) env)



-- #############################################################################
-- ** Substitutions
-- #############################################################################



-- | A substitution is a mapping from type variables to 'MType's. Applying a
-- substitution means applying those replacements. For example, the substitution
-- @a -> Int@ applied to @a -> a@ yields the result @Int -> Int@.
--
-- A key concept behind Hindley-Milner is that once we dive deeper into an
-- expression, we learn more about our type variables. We might learn that  @a@
-- has to be specialized to @b -> b@, and then later on that @b@ is actually
-- @Int@. Substitutions are an organized way of carrying this information along.
newtype Subst = Subst (Map Name MType)



-- | We're going to apply substitutions to a variety of other values that
-- somehow contain type variables, so we overload this application operation in
-- a class here.
--
-- Laws:
--
-- @
-- 'applySubst' 'mempty' ≡ 'id'
-- 'applySubst' (s1 '<>' s2) ≡ 'applySubst' s1 . 'applySubst' s2
-- @
class Substitutable a where
    applySubst :: Subst -> a -> a

instance (Substitutable a, Substitutable b) => Substitutable (a,b) where
    applySubst s (x,y) = (applySubst s x, applySubst s y)

-- | @'applySubst' s1 s2@ applies one substitution to another, replacing all the
-- bindings in the second argument @s2@ with their values mentioned in the first
-- one (@s1@).
instance Substitutable Subst where
    applySubst s (Subst target) = Subst (fmap (applySubst s) target)

-- | >>> :{
--    putPprLn (Subst
--        [ ("a", TFun "b" "b")
--        , ("b", TEither "c" "d") ])
-- :}
-- { a ––> b → b
-- , b ––> Either c d }
instance Pretty Subst where
    ppr (Subst s) = "{ " <> T.intercalate "\n, " [ ppr k <> " ––> " <> ppr v | (k,v) <- M.toList s ] <> " }"

-- | Combine two substitutions by applying all substitutions mentioned in the
-- first argument to the type variables contained in the second.
instance Monoid Subst where
    -- Considering that all we can really do with a substitution is apply it, we
    -- can use the one of 'Substitutable's laws to show that substitutions
    -- combine associatively,
    --
    -- @
    --   applySubst (compose s1 (compose s2 s3))
    -- = applySubst s1 . applySubst (compose s2 s3)
    -- = applySubst s1 . applySubst s2 . applySubst s3
    -- = applySubst (compose s1 s2) . applySubst s3
    -- = applySubst (compose (compose s1 s2) s3)
    -- @
    mappend subst1 subst2 = Subst (s1 `M.union` s2)
      where
        Subst s1 = subst1
        Subst s2 = applySubst subst1 subst2

    mempty = Subst M.empty



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
-- ** Inference context
-- #############################################################################



-- | The inference type holds a supply of unique names, and can fail with a
-- descriptive error if something goes wrong.
--
-- /Invariant:/ the supply must be infinite, or we might run out of names to
-- give to things.
newtype Infer a = Infer (ExceptT InferError (State [Name]) a)
    deriving (Functor, Applicative, Monad)

-- | Errors that can happen during the type inference process.
data InferError =
    -- | Two types that don't match were attempted to be unified.
    --
    -- For example, @a -> a@ and @Int@ do not unify.
    --
    -- >>> putPprLn (CannotUnify (TFun "a" "a") (TConst "Int"))
    -- Cannot unify a → a with Int
      CannotUnify MType MType

    -- | A 'TVar' is bound to an 'MType' that already contains it.
    --
    -- The canonical example of this is @λx. x x@, where the first @x@
    -- in the body has to have type @a -> b@, and the second one @a@. Since
    -- they're both the same @x@, this requires unification of @a@ with
    -- @a -> b@, which only works if @a = a -> b = (a -> b) -> b = …@, yielding
    -- an infinite type.
    --
    -- >>> putPprLn (OccursCheckFailed "a" (TFun "a" "a"))
    -- Occurs check failed: a already appears in a → a
    | OccursCheckFailed Name MType

    -- | The value of an unknown identifier was read.
    --
    -- >>> putPprLn (UnknownIdentifier "a")
    -- Unknown identifier: a
    | UnknownIdentifier Name
    deriving Show

-- | >>> putPprLn (CannotUnify (TEither "a" "b") (TTuple "a" "b"))
-- Cannot unify Either a b with (a, b)
instance Pretty InferError where
    ppr = \case
        CannotUnify t1 t2 ->
            "Cannot unify " <> ppr t1 <> " with " <> ppr t2
        OccursCheckFailed name ty ->
            "Occurs check failed: " <> ppr name <> " already appears in " <> ppr ty
        UnknownIdentifier name ->
            "Unknown identifier: " <> ppr name



-- | Evaluate a value in an 'Infer'ence context.
--
-- >>> let expr = EAbs "f" (EAbs "g" (EAbs "x" (EApp (EApp "f" "x") (EApp "g" "x"))))
-- >>> putPprLn expr
-- λf g x. f x (g x)
-- >>> let inferred = runInfer (infer (Env []) expr)
-- >>> let demonstrate = \case Right (_, ty) -> T.putStrLn (":: " <> ppr ty)
-- >>> demonstrate inferred
-- :: (c → e → f) → (c → e) → c → f
runInfer :: Infer a -- ^ Inference data
         -> Either InferError a
runInfer (Infer inf) =
    evalState (runExceptT inf) (map Name (infiniteSupply alphabet))
  where

    alphabet = map T.singleton ['a'..'z']

    -- [a, b, c] ==> [a,b,c, a1,b1,c1, a2,b2,c2, …]
    infiniteSupply supply = supply <> addSuffixes supply (1 :: Integer)
      where
        addSuffixes xs n = map (\x -> addSuffix x n) xs <> addSuffixes xs (n+1)
        addSuffix x n = x <> T.pack (show n)



-- | Throw an 'InferError' in an 'Infer'ence context.
--
-- >>> case runInfer (throw (UnknownIdentifier "var")) of Left err -> putPprLn err
-- Unknown identifier: var
throw :: InferError -> Infer a
throw = Infer . throwE



-- #############################################################################
-- ** Unification
-- #############################################################################

-- $ Unification describes the process of making two different types compatible
-- by specializing them where needed. A desirable property to have here is being
-- able to find the most general unifier. Luckily, we'll be able to do that in
-- our type system.



-- | The unification of two 'MType's is the most general substituion that can be
-- applied to both of them in order to yield the same result.
--
-- >>> let m1 = TFun "a" "b"
-- >>> putPprLn m1
-- a → b
-- >>> let m2 = TFun "c" (TEither "d" "e")
-- >>> putPprLn m2
-- c → Either d e
-- >>> let inferSubst = unify (m1, m2)
-- >>> case runInfer inferSubst of Right subst -> putPprLn subst
-- { a ––> c
-- , b ––> Either d e }
unify :: (MType, MType) -> Infer Subst
unify = \case
    (TFun a b,    TFun x y)          -> unifyBinary (a,b) (x,y)
    (TVar v,      x)                 -> v `bindVariableTo` x
    (x,           TVar v)            -> v `bindVariableTo` x
    (TConst a,    TConst b) | a == b -> pure mempty
    (TList a,     TList b)           -> unify (a,b)
    (TEither a b, TEither x y)       -> unifyBinary (a,b) (x,y)
    (TTuple a b,  TTuple x y)        -> unifyBinary (a,b) (x,y)
    (a, b)                           -> throw (CannotUnify a b)

  where

    -- Unification of binary type constructors, such as functions and Either.
    -- Unification is first done for the first operand, and assuming the
    -- required substitution, for the second one.
    unifyBinary :: (MType, MType) -> (MType, MType) -> Infer Subst
    unifyBinary (a,b) (x,y) = do
        s1 <- unify (a, x)
        s2 <- unify (applySubst s1 (b, y))
        pure (s1 <> s2)



-- | Build a 'Subst'itution that binds a 'Name' of a 'TVar' to an 'MType'. The
-- resulting substitution should be idempotent, i.e. applying it more than once
-- to something should not be any different from applying it only once.
--
-- - In the simplest case, this just means building a substitution that just
--   does that.
-- - Substituting a 'Name' with a 'TVar' with the same name unifies a type
--   variable with itself, and the resulting substitution does nothing new.
-- - If the 'Name' we're trying to bind to an 'MType' already occurs in that
--   'MType', the resulting substitution would not be idempotent: the 'MType'
--   would be replaced again, yielding a different result. This is known as the
--   Occurs Check.
bindVariableTo :: Name -> MType -> Infer Subst

bindVariableTo name (TVar v) | boundToSelf = pure mempty
  where
    boundToSelf = name == v

bindVariableTo name mType | name `occursIn` mType = throw (OccursCheckFailed name mType)
  where
    n `occursIn` ty = n `S.member` freeMType ty

bindVariableTo name mType = pure (Subst (M.singleton name mType))



-- #############################################################################
-- ** Type inference
-- #############################################################################

-- $ Type inference is the act of finding out a value's type by looking at the
-- environment it is in, in order to make it compatible with it.
--
-- In literature, the Hindley-Damas-Milner inference algorithm ("Algorithm W")
-- is often presented in the style of logical formulas, and below you'll find
-- that version along with code that actually does what they say.
--
-- These formulas look a bit like fractions, where the "numerator" is a
-- collection of premises, and the denominator is the consequence if all of them
-- hold.
--
-- __Example:__
--
-- @
-- Γ ⊢ even : Int → Bool   Γ ⊢ 1 : Int
-- –––––––––––––––––––––––––––––––––––
--          Γ ⊢ even 1 : Bool
-- @
--
-- means that if we have a value of type @Int -> Bool@ called "even" and a value
-- of type @Int@ called @1@, then we also have a value of type @Bool@ via
-- @even 1@ available to us.
--
-- The actual inference rules are polymorphic versions of this example, and
-- the code comments will explain each step in detail.



-- -----------------------------------------------------------------------------
-- *** The language: typed lambda calculus
-- -----------------------------------------------------------------------------



-- | The syntax tree of the language we'd like to typecheck. You can view it as
-- a close relative to simply typed lambda calculus, having only the most
-- necessary syntax elements.
--
-- Since 'ELet' is non-recursive, the usual fixed-point function
-- @fix : (a → a) → a@ can be introduced to allow recursive definitions.
data Exp = ELit Lit          -- ^ True, 1
         | EVar Name         -- ^ @x@
         | EApp Exp Exp      -- ^ @f x@
         | EAbs Name Exp     -- ^ @λx. e@
         | ELet Name Exp Exp -- ^ @let x = e in e'@ (non-recursive)
         deriving Show



-- | Literals we'd like to support. Since we can't define new data types in our
-- simple type system, we'll have to hard-code the possible ones here.
data Lit = LBool Bool
         | LInteger Integer
         deriving Show



-- | >>> putPprLn (EAbs "f" (EAbs "g" (EAbs "x" (EApp (EApp "f" "x") (EApp "g" "x")))))
-- λf g x. f x (g x)
instance Pretty Exp where
    ppr (ELit lit) = ppr lit

    ppr (EVar name) = ppr name

    ppr (EApp f x) = pprApp1 f <> " " <> pprApp2 x
      where
        pprApp1 = \case
            eLet@ELet{} -> "(" <> ppr eLet <> ")"
            eLet@EAbs{} -> "(" <> ppr eLet <> ")"
            e -> ppr e
        pprApp2 = \case
            eApp@EApp{} -> "(" <> ppr eApp <> ")"
            e -> pprApp1 e

    ppr x@EAbs{} = pprAbs True x
      where
        pprAbs True  (EAbs name expr) = "λ" <> ppr name <> pprAbs False expr
        pprAbs False (EAbs name expr) = " " <> ppr name <> pprAbs False expr
        pprAbs _ expr                 = ". " <> ppr expr

    ppr (ELet name value body) =
        "let " <> ppr name <> " = " <> ppr value <> " in " <> ppr body

-- | >>> putPprLn (LBool True)
-- True
--
-- >>> putPprLn (LInteger 127)
-- 127
instance Pretty Lit where
    ppr = \case
        LBool    b -> showT b
        LInteger i -> showT i
      where
        showT :: Show a => a -> Text
        showT = T.pack . show

-- | >>> "var" :: Exp
-- EVar (Name "var")
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
    drawFromSupply = Infer (do
        s:upply <- lift get
        lift (put upply)
        pure (Right s) )



-- | Add a new binding to the environment.
--
-- The Haskell equivalent would be defining a new value, for example in module
-- scope or in a @let@ block. This corresponds to the "comma" operation used in
-- formal notation,
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
-- 'Exp's 'MType' along with a substitution that has to be done in order to reach
-- this goal.
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
inferLit lit = pure (mempty, TConst litTy)
  where
    litTy = case lit of
        LBool    {} -> "Bool"
        LInteger {} -> "Integer"



-- | Inferring the type of a variable is done via
--
-- @
-- x:σ ∈ Γ   τ = instantiate(σ)
-- ––––––––––––––––––––––––––––  [Var]
--            Γ ⊢ x:τ
-- @
--
-- This means that if @Γ@ /literally contains/ (@∈@) a value, then it also
-- /entails it/ (@⊢@) in all its instantiations.
inferVar :: Env -> Name -> Infer (Subst, MType)
inferVar env name = do
    sigma <- lookupEnv env name -- x:σ ∈ Γ
    tau <- instantiate sigma    -- τ = instantiate(σ)
                                -- ------------------
    pure (mempty, tau)          -- Γ ⊢ x:τ



-- | Look up the 'PType' of a 'Name' in the 'Env'ironment.
--
-- This checks whether @x:σ@ is /literally contained/ in @Γ@. For more details
-- about this, see the documentation of 'Env'.
--
-- To give a Haskell analogon, looking up @id@ when @Prelude@ is loaded, the
-- resulting 'PType' would be @id@'s type, namely @forall a. a -> a@.
lookupEnv :: Env -> Name -> Infer PType
lookupEnv (Env env) name = case M.lookup name env of
    Just x  -> pure x
    Nothing -> throw (UnknownIdentifier name)



-- | Bind all quantified variables of a 'PType' to 'fresh' type variables.
--
-- __Example:__ instantiating @forall a. a -> b -> a@ results in the 'MType'
-- @c -> b -> c@, where @c@ is a fresh name (to avoid shadowing issues).
--
-- You can picture the 'PType' to be the prototype converted to an instantiated
-- 'MType', which can now be used in the unification process.
--
-- Another way of looking at it is by simply forgetting which variables were
-- quantified, carefully avoiding name clashes when doing so.
--
-- 'instantiate' can also be seen as the opposite of 'generalize', which we'll
-- need later to convert an 'MType' to a 'PType'.
instantiate :: PType -> Infer MType
instantiate (Forall qs t) = do
    subst <- substituteAllWithFresh qs
    pure (applySubst subst t)

  where
    -- For each given name, add a substitution from that name to a fresh type
    -- variable to the result.
    substituteAllWithFresh :: Set Name -> Infer Subst
    substituteAllWithFresh xs = do
        let freshSubstActions = M.fromSet (const fresh) xs
        freshSubsts <- sequenceA freshSubstActions
        pure (Subst freshSubsts)



-- | Function application captures the fact that if we have a function and an
-- argument we can give to that function, we also have the result value of the
-- result type available to us.
--
-- @
-- Γ ⊢ f : fτ   Γ ⊢ x : xτ   fxτ = fresh   unify(fτ, xτ → fxτ)
-- –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––  [App]
--                       Γ ⊢ f x : fxτ
-- @
--
-- This rule says that given a function and a value with a type, the function
-- type has to unify with a function type that allows the value type to be its
-- argument.
inferApp
    :: Env
    -> Exp -- ^ __f__ x
    -> Exp -- ^ f __x__
    -> Infer (Subst, MType)
inferApp env f x = do
    (s1, fTau) <- infer env f                         -- f : fτ
    (s2, xTau) <- infer (applySubst s1 env) x         -- x : xτ
    fxTau <- fresh                                    -- fxτ = fresh
    s3 <- unify (applySubst s2 fTau, TFun xTau fxTau) -- unify (fτ, xτ → fxτ)
    let s = s3 <> s2 <> s1                            -- --------------------
    pure (s, applySubst s3 fxTau)                     -- f x : fxτ



-- | Lambda abstraction is based on the fact that when we introduce a new
-- variable, the resulting lambda maps from that variable's type to the type of
-- the body.
--
-- @
-- τ = fresh   σ = ∀∅. τ   Γ, x:σ ⊢ e:τ'
-- –––––––––––––––––––––––––––––––––––––  [Abs]
--             Γ ⊢ λx.e : τ→τ'
-- @
--
-- Here, @Γ, x:τ@ is @Γ@ extended by one additional mapping, namely @x:τ@.
--
-- Abstraction is typed by extending the environment by a new 'MType', and if
-- under this assumption we can construct a function mapping to a value of that
-- type, we can say that the lambda takes a value and maps to it.
inferAbs
    :: Env
    -> Name -- ^ λ__x__. e
    -> Exp  -- ^ λx. __e__
    -> Infer (Subst, MType)
inferAbs env x e = do
    tau <- fresh                           -- τ = fresh
    let sigma = Forall [] tau              -- σ = ∀∅. τ
        env' = extendEnv env (x, sigma)    -- Γ, x:σ …
    (s, tau') <- infer env' e              --        … ⊢ e:τ'
                                           -- ---------------
    pure (s, TFun (applySubst s tau) tau') -- λx.e : τ→τ'



-- | A let binding allows extending the environment with new bindings in a
-- principled manner. To do this, we first have to typecheck the expression to
-- be introduced. The result of this is then generalized to a 'PType', since let
-- bindings introduce new polymorphic values, which are then added to the
-- environment. Now we can finally typecheck the body of the "in" part of the
-- let binding.
--
-- Note that in our simple language, let is non-recursive, but recursion can be
-- introduced as usual by adding a primitive @fix : (a → a) → a@ if desired.
--
-- @
-- Γ ⊢ e:τ   σ = gen(Γ,τ)   Γ, x:σ ⊢ e':τ'
-- –––––––––––––––––––––––––––––––––––––––  [Let]
--         Γ ⊢ let x = e in e' : τ'
-- @
inferLet
    :: Env
    -> Name -- ^ let __x__ = e in e'
    -> Exp -- ^ let x = __e__ in e'
    -> Exp -- ^ let x = e in __e'__
    -> Infer (Subst, MType)
inferLet env x e e' = do
    (s1, tau) <- infer env e              -- Γ ⊢ e:τ
    let env' = applySubst s1 env
    let sigma = generalize env' tau       -- σ = gen(Γ,τ)
    let env'' = extendEnv env' (x, sigma) -- Γ, x:σ
    (s2, tau') <- infer env'' e'          -- Γ ⊢ …
                                          -- --------------------------
    pure (s2 <> s1, tau')                 --     … let x = e in e' : τ'



-- | Generalize an 'MType' to a 'PType' by universally quantifying over all the
-- type variables contained in it, except those already free in the environment.
--
-- >>> let tau = TFun "a" (TFun "b" "a")
-- >>> putPprLn tau
-- a → b → a
-- >>> putPprLn (generalize (Env [("x", Forall [] "b")]) tau)
-- ∀a. a → b → a
--
-- In more formal notation,
--
-- @
-- gen(Γ,τ) = ∀{α}. τ
--     where {α} = free(τ) – free(Γ)
-- @
--
-- 'generalize' can also be seen as the opposite of 'instantiate', which
-- converts a 'PType' to an 'MType'.
generalize :: Env -> MType -> PType
generalize env mType = Forall qs mType
  where
    qs = freeMType mType `S.difference` freeEnv env
