This repository is filled with articles I ([quchen][q]) have written and needed online for various reasons.

[q]: https://github.com/quchen/articles

- **applicative_monad.md** is the original text of the 2013 Applicative-Monad proposal, which finally got things going to make Applicative a superclass of Monad.
- **build.md** explains how the `build` function, which is used in rewrite rules for list-based functions, works.
- **cont_monad.md** is my shot at explaining the `Cont` Monad.
- **crazy_io.md** shows some examples of lazy IO pitfalls in Haskell.
- **fbut.md** is a list of **F**requently **B**rought **U**p **T**opics in #haskell for easy reference. It's like an FAQ, except that the "F" stands for "frequently" instead of "someone thought this may be worth mentioning".
- **great_things_about_haskell.md** explains what I thought was cool about Haskell some time ago. Maybe I should update it some day.
- **install_haskell_platform_manually.md** was once a small file for myself to walk me through setting up GHC properly in case I needed to. The revised version is this file, explaining how to get from zero to GHC and Haskell Platform.
- **law-rules.md** is a draft of a not-yet-proposed proposal for a GHC feature.
- **loeb-moeb.md** is about an interesting function that calculates the result of a function when applied to the result of the function.
- **modular_keyboard.md** describes a keyboard I'd like to have. I encourage everyone to steal this idea so I can buy one from him.
- **monad_fail.md** is a proposal similar to the AMP, with the goal of removing `fail` from the `Monad` typeclass.
- **reader_instance_derived.md** shows step by step how the golfed instance of Reader (as presented in LYAH, for example) can be derived from something sensible and readable.
- **useful_techniques.md** shows some neat little Haskell tricks I've collected.
- **write_yourself_a_brainfuck.md** is a tutorial on how to write a basic [Brainfuck][bf] interpreter in Haskell. It also includes enough exercises to keep readers busy.
- **zero_to_local_ghc.md** is the advanced version of *install_haskell_platform_manually.md* from above: it guides through the installation of self-compiled GHC plus standard libraries and tools from nothing.

[bf]: https://en.wikipedia.org/wiki/Brainfuck