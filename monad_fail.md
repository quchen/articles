`MonadFail` proposal (MFP)
==========================

A couple of years ago, we proposed to make `Applicative` a superclass of
`Monad`, which successfully killed the single most ugly thing in Haskell
as of GHC 7.10.

Now, it's time to tackle the other major issue with `Monad`: `fail` being a
part of it.

You can contact me as usual via IRC/Freenode as *quchen*, or by email to
*dluposchainsky at the email service of Google*.



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
    mfail :: String -> m a
```

Desugaring can now be changed to produce this constraint when necessary. For
this, we have to decide when a pattern match can not fail; if this is the case,
we can omit inserting the `mfail` call.

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
   more                           >>>         f _ = mfail "..."
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
   more                         >>>         f _ = mfail "..."
                                >>>     in  action >>= f

do (view -> ~pat) <- action     >>>     let f (view -> ~pat) = more
   more                         >>>     in  action >>= f
```

A similar issue arises for `PatternSynonyms`, which we cannot inspect during
compilation sufficiently. A pattern synonym will therefore always be considered
failable.

```haskell
do PatternSynonym x <- action     >>>     let f PatternSynonym x = more
   more                           >>>     in f _ = mfail "..."
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

- What laws should `mfail` follow? **Left zero**,

  ```haskell
  ∀ s f.  fail s >>= f  ≡  fail s
  ```

  A call to `mfail` should abort the computation. In this sense, `mfail` would
  become a close relative of `mzero`. It would work well with the common
  definition `mfail _ = mzero`, and give a simple guideline to the intended
  usage and effect of the `MonadFail` class.

- Backwards compatibility with some old modules will be broken; I don't see a
  way around this. Warnings and sufficient time to fix them will have to do.

- Rename `fail`? **Yes.** Introducing `mfail` allows us to do a smooth
  transition easily (see below section), and removing the "m" again afterwards
  is simply not worth the hassle.

- Remove the `String` argument? **No.** The `String` might help error reporting
  and debugging. `String` may be ugly, but it's the de facto standard for
  simple text in GHC. Also, no high performance string operations are to be
  expected with `mfail`, so this breaking change would in no way be justified.

- How sensitive would existing code be to subtle changes in the strictness
  behaviour of `do` notation pattern matching? **It doesn't.** The
  implementation does not affect strictness at all, only the desugaring step.
  Care must be taken when fixing warnings by making patterns irrefutable using
  `~`, as that does affect strictness. (Cf. difference between lazy/strict
  State)

- The `Monad` constraint for `MonadFail` is completely unnecessary. What other
  things should be considered?

  - Applicative `do` notation is coming sooner or later, `fail` might be useful
    in this more general scenario. Due to the AMP, it is trivial to change
    the `MonadFail` superclass to `Applicative` later.
  - The class might be misused for a strange pointed type if left without
    any constraint. The docs will have to make it clear that this is not the
    intended use.

  I think we should keep the `Monad` superclass for two main reasons:

  - We don't want to see `(Monad m, MonadFail m) =>` all over the place.
  - The primary intended use of `mfail` is for desugaring do-notation anyway.
  - Retroactively removing superclasses is easy, but adding them is hard
    (see AMP).

- Provide `mfail = fail` as a standard implementation? **No.** We want a
  warning to happen and people should actively write the `MonadFail` instance
  instead of relying on defaults, so that after a while we can simply remove
  `fail`.

- Move `fail` out of the `Monad` class into a top-level synonym for `mfail`?
  **Maybe.** `fail` is less bound to being in a monadic context (since it also
  makes sense in an `Applicative` or even `Functor` one). On the other hand,
  being there for desugaring do-notation, it is likely to appear mostly in
  monadic contexts anyway.

- Whether a pattern is unfailable is up to GHC to decide, and in fact the
  compiler already does that decision in the typechecker: an unfailable pattern
  is currently ignored by the typechecker. [(Source)][ghc-typecheck-irrefutable]




Adapting old code
-----------------

- Help! My code is broken because of a missing `MonadFail` instance!

  *Here are your options:*

    1. Write a `MonadFail` instance

    2. Change your pattern to be irrefutable

    3. Emulate the old behaviour by desugaring the pattern match by hand:

       ```haskell
       do Left e <- foobar
          stuff
       ```

       becomes

       ```haskell
       do x <- foobar
          e <- case foobar of
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
logs for found "invalid use of fail desugaring". Assuming my implementation
is correct, the number of "missing `MonadFail`" warnings generated is 487.
Note that I filtered out `[]`, `Maybe` and `ReadPrec`, since those can be given
a `MonadFail` instance from within GHC, and no breakage is expected from them.

The build logs can be found [here][stackage-logs]. Search for "failable
pattern" to find your way to the still pretty raw warnings.




Transitional strategy
---------------------

The roadmap is similar to the [AMP][amp], the main difference being that since
`mfail` does not exist yet, we have to introduce new functionality and then
switch to it.

1. GHC 7.12

    - Add `MonadFail` with `mfail` so people can start writing instances
      for it.
    - Add a language extension `-XMonadFail` that changes desugaring to use
      `mfail` instead of `fail`.
    - Warn when a do-block that contains a failable pattern is desugared, but
      there is no `MonadFail` available: "Please add the instance or change
      your pattern matching." Add a flag to control whether this warning
      appears.
    - Warn when an instance implements the `fail` function, as it will be
      removed from the class in the future.

3. GHC 7.14

    - Switch `-XMonadFail` on by default.
    - Remove the desugaring warnings.

3. GHC 7.16

    - Remove `-XMonadFail`, leaving its effects on at all times.
    - Remove `fail` from `Monad`.
    - Provide a deprecated (!) top-level `fail = mfail` to stay compatible with
      code that explicitly uses `fail`.



Current status
--------------

- [ZuriHac 2015 (29.5. - 31.5.)][zurihac]: Franz Thoma (@fmthoma) and me
  (David Luposchainsky aka @quchen) started implementing the MFP in GHC.

    - Desugaring to the new `mfail` can be controlled via a new langauge
      extension, `MonadFailDesugaring`.
    - If the language extension is turned off, a warning will be emitted for
      code that would break if it was enabled.
    - Warnings are emitted for types that *have* a *MonadFail* instance. This
      still needs to be fixed.
    - The error message are readable, but should be more so. We're still
      on this.
- 2015-06-09: Estimated breakage by compiling Stackage. Smaller than expected.



[amp]: https://github.com/quchen/articles/blob/master/applicative_monad.md
[ghc-typecheck-irrefutable]: https://github.com/ghc/ghc/blob/228ddb95ee137e7cef02dcfe2521233892dd61e0/compiler/hsSyn/HsPat.hs#L443
[stackage-logs]: https://www.dropbox.com/s/knz0i979skam4zs/stackage-build.tar.xz?dl=0
[zurihac]: https://wiki.haskell.org/ZuriHac2015
