Quchen's Haskell code style
===========================


This describes my Haskell code style and the rationale behind it.
**This article is not polished or finished at all, I add stuff as I come up
with it.**


Indentation, alignment, whitespace
----------------------------------

- One indentation level is 6 spaces. It aligns nicely with `where` clauses and
  I find 4 spaces too small to tell indentation levels apart. If not for the
  `where` I would probably use 8 spaces.

  ```haskell
  main = do
        name <- getLine
        putStrLn ("Hello, " ++ name)
  ```

  'where' placement depends on the main focus of a definition: is it after the
  first "=", or inside the 'where'?

  ```haskell
  -- 'where' centric: Small main function body using lots of 'where' definitions.
  -- This style keeps the indentation level of the sub-definitions low.
  where1 = definition where
        foo = <...>
        bar = <...>
        <...>

  -- Top centric: 'where' defines auxiliary values, but the main work (actually
  -- or conceptually) is done at the topmost level.
  where2 = definition
        where foo = <...>
  ```

- Literal tabs are syntax errors.

- Whenever expressions are short and the current indentation level isn't too
  large, expressions continue on the same column as the start of the block.

  ```haskell
  main = do name <- getLine
            putStrLn ("Hello, " ++ name)
  ```

- `let` definitions are aligned with the `in` body, which requires an additional
  space after the `in`.

  ```haskell
  main = let hw = "Hello, world!"
         in  putStrLn hw
  ```

- Whenever the positions of function arguments or operators like `->` or `=` are
  in about the same column, they should be aligned.

  ```haskell
  multiple    = ...
  definitions = ...
  together    = ...

  case x of
        Left  l -> ...
        Right r -> ...
  ```

- Operators should be grouped using spaces (or their absence).

  ```haskell
  collatz n | n <= 1 = [n]
  collatz n = let n' | even n    = n `quot` 2
                     | otherwise = 3*n + 1
              in  n' : collatz n'
  ```



Parentheses
-----------

- I use what I like to refer to as "sane Lisp style": use parentheses and
  general Lisp-style indentation, but don't leave out infix operators in the
  process. This makes a nice hybrid between the usual Lisp and the usual Haskell
  styles you see around.

  ```haskell
  when (a && b)
       (modifyTVar status
                   (insert key value))
  ```

- `$` is reserved for functions that typically wrap large computations and
  appear near the top level, like `atomically`, `lift` or `try`. Introducing
  another parenthesis for them seems unnecessary, as they mostly account for an
  expression's context, and now that it does by itself. In that sense `$` is
  sort of a divider between infrastructure and program logic.



Naming
------

- Name your unused patterns. An underscore is often hard to see, and naming it
  makes it explicit which part of the data you're not using.

  ```haskell
  case foo of
        Just (Left (_filename, handle)) -> doSomething handle
        _else -> ...
  ```

- Specify imports block-wise, separated by a blank line, in several sections:

  - Base
  - Semi-standard (e.g. included in the Haskell Platform)
  - Other
  - Locally defined, i.e. part of the current project

  This can sometimes be divided up some more, for example the Lens/Pipes
  ecosystems may get their own blocks.



Type signatures
---------------

- Types always have the form `name ::` with a single space, making it much
  easier to find the definition of something in a large source file.

- Long types can be broken into multiple lines that each start with an arrow:

  ```haskell
  liftM :: Monad m
        => (a -> b)
        -> m a
        -> m b
  ```



API usage
---------

- For some basic functions, prefer monomorphic implementations over overloaded
  ones whenever this is possible without loss of generality.

  ```haskell
  -- Mapping over lists
  map f [a,b,c]
  -- instead of
  fmap f [a,b,c]

  -- Chaining functions
  fmap . fmap
  -- instead of
  fmap fmap fmap

  -- Concatenating Strings
  "hello" ++ "world"
  -- instead of
  "hello" <> "world"
  ```

  Note that there is only a handful of functions where I recommend doing this;
  the list above is almost exhaustive.

- Advanced functions, like `traverse`, should always be used in their
  generalized form. It would just be a burden to the reader to keep all the
  locally defined `traverseTYPE` functions in mind, and there is no benefit
  for doing so.

- Import modules with names conflicting with basic modules, such as `Prelude`
  and `Data.List`, qualified; do this regardless of whether the conflicting
  basic modules are actually imported.

  ```haskell
  import qualified Data.Foldable as F
  -- instead of
  import Prelude hiding (foldl, foldr, ...)
  import Data.Foldable
  ```
