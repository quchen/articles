{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where



import qualified Data.Map     as M
import           Data.Monoid
import           Data.Text    (Text)
import qualified Data.Text.IO as T

import HindleyMilner



-- #############################################################################
-- #############################################################################
-- * Testing
-- #############################################################################
-- #############################################################################



-- #############################################################################
-- ** A small custom Prelude
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
    , ("[]",         Forall ["a"]           (TList "a"))
    , ("(:)",        Forall ["a"]           ("a" ~> TList "a" ~> TList "a"))
    ])
  where
    tBool = TConst "Bool"
    tInteger = TConst "Integer"
    tMaybe = TEither (TConst "()")



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



-- #############################################################################
-- ** Run it!
-- #############################################################################



-- | Run type inference on a cuple of values
main :: IO ()
main = do
    let inferAndPrint = T.putStrLn . ("  " <>) . showType prelude
    T.putStrLn "Well-typed:"
    do
        inferAndPrint (lambda ["x"] "x")
        inferAndPrint (lambda ["f","g","x"] (apply "f" ["x", apply "g" ["x"]]))
        inferAndPrint (lambda ["f","g","x"] (apply "f" [apply "g" ["x"]]))
        inferAndPrint (lambda ["m", "k", "c"] (apply "m" [lambda ["x"] (apply "k" ["x", "c"])])) -- >>= for Cont
        inferAndPrint (lambda ["f"] (apply "(.)" ["reverse", apply "map" ["f"]]))
        inferAndPrint (apply "find" [lambda ["x"] (apply "(>)" ["x", int 0])])
        inferAndPrint (apply "map" [apply "map" ["map"]])
        inferAndPrint (apply "(*)" [int 1, int 2])
        inferAndPrint (apply "foldr" ["(+)", int 0])
        inferAndPrint (apply "map" ["length"])
        inferAndPrint (apply "map" ["map"])
        inferAndPrint (lambda ["x"] (apply "ifThenElse" [apply "(<)" ["x", int 0], int 0, "x"]))
        inferAndPrint (lambda ["x"] (apply "fix" [lambda ["xs"] (apply "(:)" ["x", "xs"])]))
    T.putStrLn "Ill-typed:"
    do
        inferAndPrint (apply "(*)" [int 1, bool True])
        inferAndPrint (apply "foldr" [int 1])
        inferAndPrint (lambda ["x"] (apply "x" ["x"]))
        inferAndPrint (lambda ["x"] (ELet "xs" (apply "(:)" ["x", "xs"]) "xs"))



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



-- | Convenience function to run type inference algorithm
showType :: Env    -- ^ Starting environment, e.g. 'prelude'.
         -> Exp    -- ^ Expression to typecheck
         -> Text   -- ^ Text representation of the result. Contains an error
                   --   message on failure.
showType env expr =
    case (runInfer . fmap (generalize (Env mempty) . uncurry applySubst) . infer env) expr of
        Left err -> "Error inferring type of " <> ppr expr <>": " <> ppr err
        Right ty -> ppr expr <> " :: " <> ppr ty
