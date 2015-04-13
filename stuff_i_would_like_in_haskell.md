Stuff I'd like to see done in/for Haskell
=========================================

This is a list of things I think would be useful to have that have something to
do with Haskell.

- GHC

    1. Error hints. "Missing instance Num" is completely unhelpful for
       beginners, and among the ten lines generated, another one of sorts of
       "maybe you forgot an argument" would certainly not hurt.


- GHCi

    1. Colours.

    2. Ability to suppress messages like

        ```
        Loading package ghc-prim ... linking ... done.
        Loading package integer-gmp ... linking ... done.
        Loading package base ... linking ... done.
        ```

       when loading packages in GHCi. Importing something from Edwardverse
       is embarassing in presentations.

    3. Vim syntax (jumping from Zsh to GHCi is a pain).


- Haddock

    1. Type signatures in the index overview

    2. Better syntax (CommonMark?)

    3. Ability to hide "missing doc" messages via `OPTIONS_HADDOCK` pragma,
       like the [other module flags already present][hmf]


- Cabal

    1. Support for standalone installations of binary files. There are scripts
       that work around this issue, [one by myself][cib], but it should really
       be part of Cabal.

[hmf]: https://www.haskell.org/haddock/doc/html/module-attributes.html
[cib]: https://github.com/quchen/cabal-install-bin/