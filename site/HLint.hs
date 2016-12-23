import "hint" HLint.Default
import "hint" HLint.Builtin.All

-- Avoiding lambdas is not always a good idea if you keep in mind that GHC's
-- inliner will only consider fully applied functions.
ignore "Avoid lambda"

-- Naming can be useful
ignore "Eta reduce"
ignore "Redundant lambda"
ignore "Use const"

-- Sometimes, it can make code more readable if underscores are allowed as a
-- form of a "large" separator. For example, one might give all tests the prefix
-- "test_", followed by a camel-case test name.
ignore "Use camelCase"

--
ignore "Use fromMaybe"

-- I have no idea why "if" is even in Haskell. I certainly do not encourage
-- using it (over a simple "case").
ignore "Use if"

-- Obfuscation much?
ignore "Use uncurry"

-- AMP fallout
error "generalize mapM"  = mapM  ==> traverse
error "generalize mapM_" = mapM_ ==> traverse_
error "generalize forM"  = forM  ==> for
error "generalize forM_" = forM_ ==> for_
error "Avoid return" =
    return ==> pure
    where note = "return is obsolete as of GHC 7.10"

-- Avoid operator soup
error "Use parentheses instead of ($)" = f $ x ==> ()
