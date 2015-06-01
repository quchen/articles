`MonadFail` proposal (MFP)
==========================

A couple of years ago, I proposed to make `Applicative` a superclass of
`Monad`, which successfully killed the single most ugly thing in Haskell
as of GHC 7.10.

Now, it's time to tackle the other major issue with `Monad`: `fail` being a
part of it.



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
  should be created instead of just using that. A parser might fail with an
  error message involving positional information, and for STM failure uses the
  default `fail = error` although it is `MonadPlus`.

- Backwards compatibility with old modules will be broken; I don't see a
  way around this.

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

- The `Monad` constraint for `MonadFail` is completely unnecessary. What other
  things should be considered?

  - Applicative `do` notation is coming sooner or later, `fail` might be useful
    in this more general scenario. Due to the AMP, it is trivial to change
    the `MonadFail` superclass to `Applicative` later. The name `mfail` will
    be a bit out of place then though.
  - The class might be misused for a strange pointed type if left without
    any constraint. The docs will have to make it clear that this is not the
    intended use.

- What laws should `mfail` follow? **Left zero**,
    ```haskell
    ∀ s f.  fail s >>= f  ≡  fail s
    ```
  A call to `mfail` should abort the computation. In this sense, `mfail` would
  become a close relative of `mzero`. It would work well with the common
  definition `mfail _ = mzero`, and give a simple guideline to the intended
  usage and effect of the `MonadFail` class.

- Provide `mfail = fail` as a standard implementation? **No.** We want a
  warning to happen and people should actively write the `MonadFail` instance
  instead of relying on defaults, so that after a while we can simply remove
  `fail`.

- Move `fail` out of the `Monad` class into a top-level synonym for `mfail`?
  **No.** There's no reason to do this: when you don't have a `MonadFail`
  instance `fail` will not typecheck, and when you have one you should simply
  write `mfail`.

- Whether a pattern is unfailable is up to GHC to decide, [and in fact the
  compiler already does that decision in the typechecker][ghc-manual-irrefutable].

[ghc-manual-irrefutable]: https://github.com/ghc/ghc/blob/228ddb95ee137e7cef02dcfe2521233892dd61e0/compiler/hsSyn/HsPat.hs#L443



Fixing broken code
------------------

Help! My code is broken because of a missing `MonadFail` instance!

*Here are your options:*

1. Write a `MonadFail` instance

2. Change your pattern to be irrefutable

3. Bind to your value, and then match against it in a separate `case` manually:

    ```haskell
    do Left e <- foobar
       stuff
    ```

  becomes

    ```haskell
    do x <- foobar
       e <- case foobar of
           Left e' -> e'
           Right r -> -- Do something useful
       stuff
    ```

  The point is you'll have to do your dirty laundry yourself now if you have
  a value that *you* know will always match, and if you don't handle the other
  patterns you'll get incompleteness warnings, and the compiler won't silently
  eat those for you.

Help! My code is broken because you removed `fail`, but my `Monad` defines it!

*Delete that definition.*



Applying the change
-------------------

The roadmap is similar to the AMP, the main difference being that since `mfail`
does not exist yet, we have to introduce new functionality and then switch to
it.

1. Preliminaries, planned to ship with GHC 7.12

    - Add `MonadFail` with `mfail` so people can start writing instances
      for it.
    - Warn when a do-block that contains a failable pattern is desugared, but
      there is no `MonadFail` available: "Please add the instance or change
      your pattern matching."
    - Warn when a type implements the `fail` function, as it will be removed
      in the future.

2. On GHC 7.12 release

    - With GHC 7.12: People get warnings and should fix their code
    - Some time after that, in GHC 7.13: Change desugaring to use
      `mfail` instead, remove `fail`. Don't do it right away or a lot of
      Hackage packages won't build with HEAD.

3. The switch in GHC 7.14

    - Roll out the previous 7.13 changes to user land



Current status
--------------

- [ZuriHac 2015 (29.5. - 31.5.)][zurihac]: Franz Thoma (@fmthoma) and me
  (David Luposchainsky aka @quchen) started implementing the MFP in GHC.

    - The actual change is in, and designed to be a trivial switch in the code.
      It is of course turned off for now.
    - A flag that allows switching the warnings on and off is there as well.
    - Warnings are being generated for failable patterns.
    - Warnings are still emitted for types that *have* a *MonadFail* instance.
    - The error message is very hacky, and we're working on making it more
      helpful.

Other things to do: probe impact of the change on Hackage. Nothing has been
done in this region so far, so that **the change should be considered experimental**.

[zurihac]: https://wiki.haskell.org/ZuriHac2015
