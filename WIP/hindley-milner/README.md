This folder serves as the Cabal package belonging to ../HindleyMilner.hs in
case you want to run it.

For convenience, the generated Haddock output (including highlighted source)
is also included here.





| This module is an extensively documented walkthrough for typechecking a
basic functional language using the Hindley-Damas-Milner algorithm.
In the end, we'll be able to infer the type of expressions like

```haskell
find (λx. (>) x 0)
>>> :: [Integer] -> Either () Integer
```

It can be used in four different forms:
- The source is written in literate programming style, so you can almost
  read it from top to bottom, minus some few references to later topics.
- The code is runnable in GHCi.
- A small main module that gives many examples of what you might try out in GHCi
  is also included.
- The Haddock output yields a nice overview over the definitions given.
  It's not as good of a read as the source since many of the important
  inter-code comments are not visible, but nice to keep open as a reference.
