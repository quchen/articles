`MonadFail` proposal (MFP)
==========================

A couple of years ago, we proposed to make `Applicative` a superclass of
`Monad`, which successfully killed the single most ugly thing in Haskell
as of GHC 7.10.

Now, it's time to tackle the other major issue with `Monad`: `fail` being a
part of it.

You can contact me as usual via IRC/Freenode as *quchen*, or by email to
*dluposchainsky at the email service of Google*. This file was posted
on the ghc-devs@ and [libraries@ mailing lists][libs-at], as well as on
[Reddit][reddit].

[reddit]: http://www.reddit.com/r/haskell/comments/397k1a/monadfail_proposal_mfp_moving_fail_out_of_monad/
[libs-at]: http://permalink.gmane.org/gmane.comp.lang.haskell.libraries/24910



Overview
--------

- **The problem** - reason for the proposal
- **MonadFail class** - the solution
- **Discussion** - explaining our design choices
- **Adapting old code** - how to prepare current code to transition smoothly
- **Esimating the breakage** - how much stuff we will break
- **Transitional strategy** - how to break as little as possible while transitioning
- **Current status**




The problem
-----------

Currently, the `<-` symbol is unconditionally desugared as follows:

```haskell
do pat <- computation     >>>     let f pat = more
   more                   >>>         f _ = fail "..."
                          >>>     in  computation >>= f
```

The problem with this is that `fail` cannot (!) be sensibly implemented for
many monads, for example `State`, `IO`, `Reader`. In those cases it defaults to
`error`. As a consequence, in current Haskell, you can not use
`Monad`-polymorphic code safely, because although it claims to work for all
`Monad`s, it might just crash on you. This kind of implicit non-totality baked
into the class is *terrible*.

The goal of this proposal is adding the `fail` only when necessary and
reflecting that in the type signature of the `do` block, so that it can be used
safely, and more importantly, is guaranteed not to be used if the type
signature does not say so.



`MonadFail` class
-----------------

To fix this, introduce a new typeclass:

```haskell
class Monad m => MonadFail m where
    fail :: String -> m a
```

Desugaring can now be changed to produce this constraint when necessary. For
this, we have to decide when a pattern match can not fail; if this is the case,
we can omit inserting the `fail` call.

The most trivial examples of unfailable patterns are of course those that match
anywhere unconditionally,

```haskell
do x <- action     >>>     let f x = more
   more            >>>     in  action >>= f
```

In particular, the programmer can assert any pattern be unfailable by making it
irrefutable using a prefix tilde:

```haskell
do ~pat <- action     >>>     let f ~pat = more
   more               >>>     in  action >>= f
```

A class of patterns that are conditionally failable are `newtype`s, and single
constructor `data` types, which are unfailable by themselves, but may fail
if matching on their fields is done with failable paterns.

```haskell
data Newtype a = Newtype a

-- "x" cannot fail
do Newtype x <- action            >>>     let f (Newtype x) = more
   more                           >>>     in  action >>= f

-- "Just x" can fail
do Newtype (Just x) <- action     >>>     let f (Newtype (Just x)) = more
   more                           >>>         f _ = fail "..."
                                  >>>     in  action >>= f
```

`ViewPatterns` are as failable as the pattern the view is matched against.
Patterns like `(Just -> Just x)` should generate a `MonadFail` constraint even
when it's "obvious" from the view's implementation that the pattern will always
match. From an implementor's perspective, this means that only types (and their
constructors) have to be looked at, not arbitrary values (like functions),
which is impossible to do statically in general.

```haskell
do (view ->  pat) <- action     >>>     let f (view ->  pat) = more
   more                         >>>         f _ = fail "..."
                                >>>     in  action >>= f

do (view -> ~pat) <- action     >>>     let f (view -> ~pat) = more
   more                         >>>     in  action >>= f
```

A similar issue arises for `PatternSynonyms`, which we cannot inspect during
compilation sufficiently. A pattern synonym will therefore always be considered
failable.

```haskell
do PatternSynonym x <- action     >>>     let f PatternSynonym x = more
   more                           >>>     in f _ = fail "..."
                                  >>>     in  action >>= f
```



Discussion
----------

- Although for many `MonadPlus` `fail _ = mzero`, a separate `MonadFail` class
  should be created instead of just using that.

    - A parser might fail with an error message involving positional
      information. Some libraries, like `Binary`, provide `fail` as their
      only interface to fail a decoding step.

    - Although `STM` is `MonadPlus`, it uses the default `fail = error`. It
      will therefore not get a `MonadFail` instance.

- What laws should `fail` follow? **Left zero**,

  ```haskell
  ∀ s f.  fail s >>= f  ≡  fail s
  ```

  A call to `fail` should abort the computation. In this sense, `fail` would
  become a close relative of `mzero`. It would work well with the common
  definition `fail _ = mzero`, and give a simple guideline to the intended
  usage and effect of the `MonadFail` class.

- Rename `fail`? **No.** Old code might use `fail` explicitly and we might
  avoid breaking it, the Report talks about `fail`, and we have a solid
  migration strategy that does not require a renaming.

- Remove the `String` argument? **No.** The `String` might help error reporting
  and debugging. `String` may be ugly, but it's the de facto standard for
  simple text in GHC. No high performance string operations are to be
  expected with `fail`, so this breaking change would in no way be justified.
  Also note that explicit `fail` calls would break if we removed the argument.

- How sensitive would existing code be to subtle changes in the strictness
  behaviour of `do` notation pattern matching? **It doesn't.** The
  implementation does not affect strictness at all, only the desugaring step.
  Care must be taken when fixing warnings by making patterns irrefutable using
  `~`, as that *does* affect strictness. (Cf. difference between lazy/strict
  State)

- The `Monad` constraint for `MonadFail` seems unnecessary. Should we drop or
  relax it? What other things should be considered?

  - Applicative `do` notation is coming sooner or later, `fail` might be useful
    in this more general scenario. Due to the AMP, it is trivial to change
    the `MonadFail` superclass to `Applicative` later. (The name will be a bit
    misleading, but it's a very small price to pay.)
  - The class might be misused for a strange pointed type if left without
    any constraint. This is not the intended use at all.

  I think we should keep the `Monad` superclass for three main reasons:

  - We don't want to see `(Monad m, MonadFail m) =>` all over the place.
  - The primary intended use of `fail` is for desugaring do-notation anyway.
  - Retroactively removing superclasses is easy, but adding them is hard
    (see AMP).




Adapting old code
-----------------

- Help! My code is broken because of a missing `MonadFail` instance!

  *Here are your options:*

    1. Write a `MonadFail` instance (and bring it into scope)

       ```haskell
       #if !MIN_VERSION_base(4,11,0)
       -- Control.Monad.Fail import will become redundant in GHC 7.16+
       import qualified Control.Monad.Fail as Fail
       #endif
       import Control.Monad

       instance Monad Foo where
         (>>=) = <...bind impl...>
         -- NB: `return` defaults to `pure`

       #if !MIN_VERSION_base(4,11,0)
         -- Monad(fail) will be removed in GHC 7.16+
         fail = Fail.fail
       #endif

       instance MonadFail Foo where
         fail = <...fail implementation...>
       ```

    2. Change your pattern to be irrefutable

    3. Emulate the old behaviour by desugaring the pattern match by hand:

       ```haskell
       do Left e <- foobar
          stuff
       ```

       becomes

       ```haskell
       do x <- foobar
          e <- case x of
              Left e' -> e'
              Right r -> error "Pattern match failed" -- Boooo
          stuff
       ```

       The point is you'll have to do your dirty laundry yourself now if you
       have a value that *you* know will always match, and if you don't handle
       the other patterns you'll get incompleteness warnings, and the compiler
       won't silently eat those for you.

- Help! My code is broken because you removed `fail` from `Monad`, but my class
  defines it!

  *Delete that part of the instance definition.*



Esimating the breakage
----------------------

Using our initial implementation, I compiled stackage-nightly, and grepped the
logs for the warnings. Assuming my implementation is correct, the number of
"missing `MonadFail`" warnings generated is 487. Note that I filtered out `[]`,
`Maybe` and `ReadPrec`, since those can be given a `MonadFail` instance from
within GHC, and no breakage is expected from them.

The build logs can be found [here][stackage-logs]. Search for "failable
pattern" to find your way to the still pretty raw warnings.

Here are some commands you might find interesting for exploring the logs:

```bash
# List all packages generating warnings (57 of them)
grep "is used in the context" *    | \
    grep -v '(‘\[|Maybe|ReadPrec)' | \
    perl -pe 's#^(.*)\.log.*$#\1#' | \
    uniq -u

# Histogram of the breaking contexts (mostly IO and parsers)
grep "is used in the context" *                    | \
    grep -v '(‘\[|Maybe|ReadPrec)'                 | \
    perl -pe 's#^.*in the context ‘([^ ]+).*$#\1#' | \
    sort                                           | \
    uniq -c                                        | \
    sort -rg
```




Transitional strategy
---------------------

The roadmap is similar to the [AMP][amp], the main difference being that since
`MonadFail` does not exist yet, we have to introduce new functionality and then
switch to it.

* **GHC 7.12 / base-4.9**

    - Add module `Control.Monad.Fail` with new class `MonadFail(fail)` so
      people can start writing instances for it.

      `Control.Monad` only re-exports the class `MonadFail`, but not its
      `fail` method.

      NB: At this point, `Control.Monad.Fail.fail` clashes with
      `Prelude.fail` and `Control.Monad.fail`.

    - *(non-essential)* Add a language extension `-XMonadFail` that
      changes desugaring to use `MonadFail(fail)` instead of `Monad(fail)`.

      This has the effect that typechecking will infer a `MonadFail` constraint
      for `do` blocks with failable patterns, just as it is planned to do when
      the entire thing is done.

    - Warn when a `do`-block that contains a failable pattern is
      desugared, but there is no `MonadFail`-instance in scope: "Please add the
      instance or change your pattern matching." Add a flag to control whether
      this warning appears.

    - Warn when an instance implements the `fail` function (or when `fail`
      is imported as a method of `Monad`), as it will be removed from the
      `Monad` class in the future. (See also [GHC #10071][trac-10071])

3. GHC 7.14

    - Switch `-XMonadFail` on by default.
    - Remove the desugaring warnings.

3. GHC 7.16

    - Remove `-XMonadFail`, leaving its effects on at all times.
    - Remove `fail` from `Monad`.
    - Instead, re-export `Control.Monad.Fail.fail` as `Prelude.fail` and
      `Control.Monad.fail`.
    - `Control.Monad.Fail` is now a redundant module that can be considered
      deprecated.



Current status
--------------

- [ZuriHac 2015 (29.5. - 31.5.)][zurihac]: Franz Thoma (@fmthoma) and me
  (David Luposchainsky aka @quchen) started implementing the MFP in GHC.

    - Desugaring to the new `fail` can be controlled via a new langauge
      extension, `MonadFailDesugaring`.
    - If the language extension is turned off, a warning will be emitted for
      code that would break if it was enabled.
    - Warnings are emitted for types that *have* a *MonadFail* instance. This
      still needs to be fixed.
    - The error message are readable, but should be more so. We're still
      on this.
- 2015-06-09: Estimated breakage by compiling Stackage. Smaller than expected.



[amp]: https://github.com/quchen/articles/blob/master/applicative_monad.md
[stackage-logs]: https://www.dropbox.com/s/knz0i979skam4zs/stackage-build.tar.xz?dl=0
[trac-10071]: https://ghc.haskell.org/trac/ghc/ticket/10071
[zurihac]: https://wiki.haskell.org/ZuriHac2015
