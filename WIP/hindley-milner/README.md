Hindley-Damas-Milner tutorial
=============================

This module is an extensively documented walkthrough for typechecking a basic
functional language using the Hindley-Damas-Milner algorithm. In the end, we'll
be able to infer the type of expressions like

```haskell
find (λx. (>) x 0)
>>> :: [Integer] -> Either () Integer
```

It can be used in four different forms:

- The source is written in literate programming style, so you can almost read it
  from top to bottom, minus some few references to later topics.

- The code is runnable in GHCi, all definitions are exposed.

- A small main module that gives many examples of what you might try out in GHCi
  is also included.

- The Haddock output yields a nice overview over the definitions given, with a
  nice rendering of a truckload of source code comments.

For Stack users
---------------

```bash
# Play around with it
stack ghci

# Run the main module
stack build --exec hindley-milner

# Build and view Haddocks
stack haddock --open hindley-milner
```

For Cabal users
---------------

```bash
# Installation
cabal sandbox init
cabal install --dependencies-only

# Play around with it
cabal repl

# Run the main module
cabal run

# Build and view Haddocks
cabal haddock
$BROWSER dist/doc/html/hindley-milner/index.html
```
