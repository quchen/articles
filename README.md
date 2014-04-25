Quchen's articles
=================

This repository is filled with articles I ([quchen][q]) have written and needed
online for various reasons.



## Haskell related

- [applicative_monad.md][amp] is the original text of the 2013 Applicative-Monad
  proposal, which finally got things going to make Applicative a superclass of
  Monad.
- [build.md][build] explains how the `build` function, which is used in rewrite
  rules for list-based functions, works.
- [cont_monad.md][cont] is my shot at explaining the `Cont` Monad.
- [crazy_io.md][crazy-io] shows some examples of lazy IO pitfalls in Haskell.
- [fbut.md][fbut] is a list of **F**requently **B**rought **U**p **T**opics in
  Freenode's #haskell channel for easy reference. It's like an FAQ, except that
  the "F" stands for "frequently" instead of "someone thought this may be worth
  mentioning".
- [functions_comprehensible.md][comprehensible]: Basic Haskell functions written
  in easily comprehensible notation. Nice for explaining e.g. `sequence`.
- [great_things_about_haskell.md][great] explains what I thought was cool about
  Haskell some time ago. Maybe I should update it some day.
- [haskell-equality-table.html][equality] [(live version)][equality-live] was
  created in a time where it seemed to be en vogue to create tables for the `==`
  operator in various languages. It's a very boring table.
- [haskell-style.md][haskell-style] describes my Haskell code style and the
  rationale behind it.
- [install_haskell_platform_manually.md][hp] was once a small file for myself to
  walk me through setting up GHC properly in case I needed to. The revised
  version is this file, explaining how to get from zero to GHC and Haskell
  Platform.
- [law-rules.md][law-rules] is a draft of a not-yet-proposed proposal for a GHC
  feature.
- [lens-infix-operators.md][lens-infix] provides an overview of what the
  individual symbols in infix operators of the [lens][lens] library stand for.
- [loeb-moeb.md][loeb] is about an interesting function that calculates the
  result of a function when applied to the result of the function.
- [monad_fail.md][fail] is a proposal similar to the AMP, with the goal of
  removing `fail` from the `Monad` typeclass.
- [reader_instance_derived.md][reader] shows step by step how the golfed
  instance of Reader (as presented in LYAH, for example) can be derived from
  something sensible and readable.
- [seqm_proposal.md][seqm-proposal] contains the proposal text for a new basic
  Haskell function to force strictness of a monadic computation.
- [unmaintainable_haskell.md][unmaintain] describes how to write unmaintainable
  Haskell code in the spirit of [How to write unmaintainable code][unmaintain-org]
- [useful_techniques.md][useful] shows some neat little Haskell tricks I've
  collected.
- [write_yourself_a_brainfuck.md][bf-tut] is a tutorial on how to write a basic
  [Brainfuck][bf] interpreter in Haskell. It also includes enough exercises to
  keep readers busy.
- [zero_to_local_ghc.md][local-ghc] is the advanced version of
  [install_haskell_platform_manually.md][hp] from above: it guides through the
  installation of self-compiled GHC plus standard libraries and tools from
  nothing.



## Other topics

- [modular_keyboard.md][modular-keyboard] describes a keyboard I'd like to have. I encourage everyone to steal this idea so I can buy one from him.





[bf]: https://en.wikipedia.org/wiki/Brainfuck
[lens]: http://hackage.haskell.org/package/lens
[q]: https://github.com/quchen/articles
[unmaintain-org]: https://www.thc.org/root/phun/unmaintain.html

[amp]:              applicative_monad.md
[bf-tut]:           write_yourself_a_brainfuck.md
[build]:            build.md
[comprehensible]:   functions_comprehensible.md
[cont]:             cont.md
[crazy-io]:         crazy_io.md
[equality-live]:    https://rawgithub.com/quchen/articles/master/haskell-equality-table.html
[equality]:         haskell-equality-table.html
[fail]:             monad_fail.md
[fbut]:             fbut.md
[great]:            great_things_about_haskell.md
[haskell-style]:    haskell_style.md
[hp]:               install_haskell_platform_manually.md
[law-rules]:        law-rules.md
[lens-infix]:       lens-infix-operators.md
[local-ghc]:        zero_to_local_ghc.md
[loeb]:             loeb-moeb.md
[modular-keyboard]: modular_keyboard.md
[reader]:           reader_instance_derived.md
[unmaintain]:       unmaintainable_haskell.md
[useful]:           useful_techniques.md
[seqm-proposal]:    seqm_proposal.md