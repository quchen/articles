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
       more                         >>>         f _              = fail "..."
                                    >>>     in  action >>= f

    do (view -> ~pat) <- action     >>>     let f (view -> ~pat) = more
                                    >>>     in  action >>= f
    ```



- Otherwise: add `MonadFail` constraint

    ```haskell
    do pat <- computation     >>>     let f pat = more
       more                   >>>         f _   = fail "..."
                              >>>     in  computation >>= f
    ```



Discussion
----------

- Although for many `MonadPlus` `fail _ = mzero`, a separate `MonadFail` class
  should be created. A parser might fail with an error message involving
  positional information, and for STM failure uses the default `fail = error`
  although it is `MonadPlus`.

- The case of one data constructor should emit a warning if the data type is
  defined via `data` (as opposed to `newtype`): adding another data constructor
  can make patterns in unrelated modules refutable. Built-ins like tuples
  should be excluded from this rule, as they will never be changed.

- Some monads use the pattern matching to force evaluation of the binding, for
  example lazy/strict `StateT`. I'm not sure what exactly the consequences of
  the above are here; I suspect a strictness annotation or `(>>=)` instead of
  `do` notation might be sufficient.

- Getting the change to work should be boring but painless: all Monad instance
  declarations involving `fail` will break because the function is removed, and
  many monadic computations have to be annotated using `~` because they were
  written under the assumption that `fail` is never called. In both these cases
  compilation errors/warnings carry sufficient information to fix the source
  code easily.

- Backwards compatibility with many old modules will be broken; I don't see a
  way around this.



Other things to consider
------------------------

- ~~Rename `fail`?~~ **Yes.** Introducing `mfail` allows us to do a smooth
  transition easily (see below section), and removing the "m" again afterwards
  is simply not worth the hassle.

- ~~Remove the `String` argument?~~ **No.** The `String` helps might help
  error reporting and debugging. `String` may be ugly, but it's the de facto
  standard for simple text in GHC. Also, no high performance string operations
  are to be expected with `mfail`, so this breaking change would in no way be
  justified.

- How sensitive would existing code be to subtle changes in the strictness
  behaviour of `do` notation pattern matching?



Applying the change
-------------------

The roadmap is similar to the AMP, the main difference being that since `mfail`
does not exist yet, we have to introduce new functionality and then switch to
it.

1. Preliminaries. Might ship with a minor version bump in the 7.10 release
   already.

   - Add `MonadFail` with `mfail`
   - Warn when a Monad defines `fail` but has no `MonadFail` instance

2. Nag for change

   - Warn when a pattern is used that would require a `MonadFail` constraint,
     but the `Monad` has none. Make it opt-in at first (e.g. as part of `-W`),
     then switch it to on by default.

3. The switch

   - Move `fail` out of the `Monad` class into a top-level synonym for `mfail`
   - Deprecate `fail`

4. Cleanup

   - Remove the now deprecated `fail` version
