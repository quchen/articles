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

    2. Vim syntax (jumping from Zsh to GHCi is a pain).


- Haddock

    1. Type signatures in the index overview

    2. Better syntax (CommonMark?)

    3. Ability to hide "missing doc" messages via `OPTIONS_HADDOCK` pragma,
       like the [other module flags already present][hmf]


- Cabal

    1. Support for standalone installations of binary files. There are scripts
       that work around this issue, [one by myself][cib], but it should really
       be part of Cabal.


- Infrastructure

    1. A build service that modifies your package bounds and tries to compile
       the package again, in order to find out the maximum dependency version
       range that should go into the .cabal file

- Libraries

    1. A library for handling graphs with less embarassing documentation and
       code than [fgl][fgl]

[hmf]: https://www.haskell.org/haddock/doc/html/module-attributes.html
[cib]: https://github.com/quchen/cabal-install-bin/
[fgl]: http://hackage.haskell.org/package/fgl