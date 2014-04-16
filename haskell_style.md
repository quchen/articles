Quchen's Haskell code style
===========================


This describes my Haskell code style and the rationale behind it.
**This article is not polished or finished at all, I add stuff as I come up
with it.**


Indentation, alignment
----------------------

- One indentation level is 6 spaces. It aligns nicely with 'where' clauses and I
  find 4 spaces too small to tell indentation levels apart. If not for the 'where'
  I would probably use 8 spaces.

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
  large, expressions continue on the same line as the start of the block.

  ```haskell
  main = do name <- getLine
            putStrLn $ "Hello, " ++ name
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