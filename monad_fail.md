Removing `fail` from `Monad`
============================

(Just dreaming around here.)



The problem
-----------


Currently, the `<-` symbol is desugared as follows:

```haskell
do pat <- computation     >>>     let f pat = more
   more                   >>>         f _   = fail "..."
                          >>>     in  computation >>= f
```

The problem with this is that `fail` cannot be sensibly implemented for many
monads, for example `State`, `IO`, `Reader`. In those cases it defaults to
`error`, i.e. the monad has a built-in crash.



`MonadFail` class
-----------------

To fix this, introduce a new typeclass:

```haskell
class Monad m => MonadFail m where
    mfail :: String -> m a
```

Desugaring then has to be changed to produce this constraint when necessary:

- Explicitly irrefutable pattern: do not add `MonadFail` constraint

    ```haskell
    do ~pat <- action     >>>     let f ~pat = more
       more               >>>     in  action >>= f
    ```



- Only one data constructor: do not add `MonadFail` constraint. This rule
  should apply recursively for nested patterns, e.g. `Only (Only' x)`.

    ```haskell
    do (Only x) <- action     >>>     let f (Only x) = more
       more                   >>>     in  action >>= f
    ```

  In particular, this means that tuples don't produce constraints.

    ```haskell
    do (a,b) <- action     >>>     let f (a,b) = more
       more                >>>     in  action >>= f
    ```



- `ViewPatterns`: add `MonadFail` constraint depending on the pattern and *not*
  the view. In other words, patterns like `(Just -> Just x)` should generate a
  `MonadFail` constraint even when it's "obvious" from the view's
  implementation that the pattern will always match. From an implementor's
  perspective, this means that only types (and their constructors) have to be
  looked at, not arbitrary values (like functions).

    ```haskell
    do (view ->  pat) <- action     >>>     let f (view ->  pat) = more
       more                         >>>         f _              = mfail "..."
                                    >>>     in  action >>= f

    do (view -> ~pat) <- action     >>>     let f (view -> ~pat) = more
       more                         >>>     in  action >>= f
    ```



- Otherwise: add `MonadFail` constraint

    ```haskell
    do pat <- computation     >>>     let f pat = more
       more                   >>>         f _   = mfail "..."
                              >>>     in  computation >>= f
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

- Remove the `String` argument? **No.** The `String`  might help error
  reporting and debugging. `String` may be ugly, but it's the de facto standard
  for simple text in GHC. Also, no high performance string operations are to be
  expected with `mfail`, so this breaking change would in no way be justified.

- How sensitive would existing code be to subtle changes in the strictness
  behaviour of `do` notation pattern matching? **It doesn't.** The
  implementation does not affect strictness at all, only the desugaring step.

- The `Monad` constraint for `MonadFail` is completely unnecessary. What other
  things should be considered?

  - Applicative `do` notation is coming sooner or later, `fail` might be useful
    in this more general scenario. Due to the AMP, it is trivial to change
    the `MonadFail` superclass to `Applicative` later.
  - The class might be misused for a strange pointed type if left without
    any constraint. The docs will have to make it clear that this is not the
    intended use.

- What laws should `mfail` follow? The first thing that comes to mind is
  following the laws similar to `empty`/`mzero`, i.e. being an identity(ish)
  for `<|>`/`mplus` and a left zero for `<*>`/`>>=`. **None.** We can mention
  that it should maybe be `MonadPlus` oriented, but the main point is making
  failable patterns safe.

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
  The short rundown is as follows:
    - Wildcards `_`, simple variables `x` and irrefutable patterns `~pat` are
      always unfailable.
    - Pattern synonyms are always failable. GHC is conservative about this, as
      it is very hard to analyze these statically.
    - Constructors of data types that have only one are as failable as their
      subfields. For example, `Newtype a <- ...` is unfailable since `a` is,
      whereas `Newtype (Left e)` is failable since `Left e` is.
    - `data` types with multiple fields are always failable.

[ghc-manual-irrefutable]: https://github.com/ghc/ghc/blob/228ddb95ee137e7cef02dcfe2521233892dd61e0/compiler/hsSyn/HsPat.hs#L443



Fixing broken code
------------------

- Write a `MonadFail` instance
- Change your pattern to be refutable
- Bind to your value, and then match against it in a separate `case`

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

2. On GHC 7.12 release

    - With GHC 7.12: People get warnings and should fix their code
    - In GHC 7.13: Change desugaring to use `mfail` instead, remove `fail`

3. The switch in GHC 7.14

    - Roll out the previous 7.13 changes to user land
