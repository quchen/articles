[Quchen][q]'s articles
=================

This repository is filled with articles I have written and needed online for
various reasons. Below is a short summary of the articles available publically.
The repository may contain files not mentioned below, in which case they're
probably unfinished or otherwise not worth reading at this point.



Haskell related
---------------



### General, tutorials

- [Algebraic blindness][alg-blindness] is about how algebraic data types
  sometimes make Boolean Blindness even more complex.
- [The `build` function explained][build], which is an important piece in a
  popular optimization technique
- [The `Cont` monad explained][cont]
- [Some lazy IO pitfalls][crazy-io]
- [The `fix` function][fix], one possible fundamental building block of
  recursion
- [**F**requently **B**rought **U**p **T**opics in Freenode's #haskell
  channel][fbut]
- [Basic Haskell functions written in easily comprehensible
  notation.][comprehensible] Nice for explaining e.g. `sequence`.
- [What I thought was cool about Haskell some time ago.][great] I should
  update this one day.
- [Equality table of Haskell values.][equality] [(live version)][equality-live]
  Created in a time where it seemed to be en vogue to create tables for the `==`
  operator in various languages. Spoiler, it's terribly boring, because it
  should be.
- [How to fix, or avoid fixing, certain warnings][fixing-warnings]
- [My Haskell code style][haskell-style] and the rationale behind it
- [Informal description of class instances][instances] gives non-technical
  descriptions of various common class instances.
- [An extensive tutorial on the Hindley-Damas-Milner type inference algorithm][hindley-milner]
- [Lens infix operators cheat sheet][lens-infix] provides an overview of what
  the individual symbols in infix operators of the [lens][lens] library stand
  for.
- [Löb/Möb][loeb] is about an interesting, simple, complicated and quirky
  recursive function.
- [Reader instance derived][reader] shows  how to write the usual `Reader`
  monad instance starting from something readable and comprehensible.
- [The second `Functor` law is redundant][functor-law]
- [Tag, don't `type`][tag-dont-type] is about avoiding `type` synonyms, and
  using `Data.Tagged` instead.
- [How to write unmaintainable Haskell][unmaintain], inspired by
  [How to write unmaintainable code][unmaintain-org]
- [Useful techniques][useful] I've found over the years and haven't seen
  described elsewhere
- [Write yourself a Brainfuck in an hour][bf-tut] implements a very simple
  [Brainfuck][bf] interpreter in Haskell, with plenty of room to go on
  afterwards.
- [Make macros mean something – readable backwards compatibility with
  CPP][cpp-semantic-macros] talks about how to define readable macro synonyms
  for writing backwards compatible libraries.



### Proposals

- [Applicative-Monad proposal (AMP)][amp] is the original text of the 2013
  Applicative-Monad proposal, which was the starting point for Applicative
  becoming a superclass of Monad in GHC 7.10.
- [Rules for typeclass laws][law-rules] wasn't proosed yet
- The MonadFail proposal is a proposal similar to the AMP, with the goal of
  removing `fail` from the `Monad` typeclass.

    - [Original text][fail]
    - [Update after one week of discussion.][fail_update1]






[bf]: https://en.wikipedia.org/wiki/Brainfuck
[lens]: http://hackage.haskell.org/package/lens
[q]: https://github.com/quchen/articles
[unmaintain-org]: https://www.thc.org/root/phun/unmaintain.html

[alg-blindness]:    algebraic-blindness.md
[amp]:              applicative_monad.md
[bf-tut]:           write_yourself_a_brainfuck.md
[build]:            build.md
[comprehensible]:   functions_comprehensible.md
[cont]:             cont_monad.md
[cpp-semantic-macros]: haskell-cpp-compatibility.md
[crazy-io]:         crazy_io.md
[equality-live]:    http://htmlpreview.github.io/?https://github.com/quchen/articles/blob/master/haskell-equality-table.html
[equality]:         haskell-equality-table.html
[fail_update1]:     monad_fail_update1.md
[fail]:             monad_fail.md
[fbut]:             fbut.md
[fix]:              fix.md
[fixing-warnings]:  fixing-warnings.md
[functor-law]:      second_functor_law.md
[great]:            great_things_about_haskell.md
[haskell-style]:    haskell_style.md
[hindley-milner]:   hindley-milner/README.md
[instances]:        instances.md
[law-rules]:        law-rules.md
[lens-infix]:       lens-infix-operators.md
[loeb]:             loeb-moeb.md
[reader]:           reader_instance_derived.md
[seqm-proposal]:    seqm_proposal.md
[tag-dont-type]:    tag-dont-type.md
[unmaintain]:       unmaintainable_haskell.md
[useful]:           useful_techniques.md
