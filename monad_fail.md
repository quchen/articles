Removing `fail` from `Monad`
============================



The problem
-----------


Currently, the `<-` symbol is desugared as follows:

```haskell
do pat <- computation     >>>     let f pat = more
   more                   >>>         f _   = fail "..."
                          >>>     in  computation >>= f
```

The problem with this is that `fail` cannot be sensibly implemented for many monads, for example `State`, `IO`, `Reader`. In those cases it defaults to `error`, i.e. the monad has a built-in crash.



`MonadFail` class
-----------------

To fix this, introduce a new typeclass:

```haskell
class Monad m => MonadFail m where
      fail :: String -> m a
```

Desugaring is then changed to the following:

```haskell
-- Explicitly irrefutable pattern: do not add MonadFail constraint
do ~pat <- computation     >>>     let f pat = more
   more                    >>>     in  computation >>= f

-- Only one data constructor: do not add MonadFail constraint.
-- This rule should apply recursively for nested patterns,
-- e.g.  Only (Only' x).
do (Only x) <- computation     >>>     let f (Only x) = more
   more                        >>>     in  computation >>= f

-- Otherwise: add MonadFail constraint
do pat <- computation     >>>     let f pat = more
   more                   >>>         f _   = fail "..."
                          >>>     in  computation >>= f
```



Discussion
----------

- Although for many `MonadPlus` `fail _ = mzero`, a separate `MonadFail` class should be created. A parser might fail with an error message involving positional information, and for STM failure is undefined although it is `MonadPlus`.

- The case of one data constructor should emit a warning if the data type is defined via `data` (as opposed to `newtype`): adding another data constructor can make patterns in unrelated modules refutable.

- Some monads use the pattern matching to force evaluation of the binding, for example lazy/strict `StateT`. I'm not sure what exactly the consequences of the above are here; I suspect a strictness annotation or `(>>=)` instead of `do` notation might be sufficient.

- Getting the change to work should be boring but painless: all Monad instance declarations involving `fail` will break because the function is removed, and many monadic computations have to be annotated using `~` because they were written under the assumption that `fail` is never called. In both these cases compilation errors/warnings carry sufficient information to fix the source code easily.

- Backwards compatibility with many old modules will be broken; I don't see a way around this.



Other things to consider
------------------------

- Rename `fail`? It's quite a generic name that would be nice to have in APIs. `failM`? `mfail`?

- Remove the `String` argument? (May hurt error reporting on pattern mismatch ..?)

- How sensitive would existing code be to subtle changes in the strictness behaviour of `do` notation pattern matching?



Applying the change
-------------------

Like the AMP,

1. Implement ad-hoc warnings that code will receive a `MonadFail` constraint in a future version. "Either make the patterns irrefutable, or keep in mind that the next compiler version will require a `MonadFail` instance". Since `MonadFail` does not clash with an existing name, it could be introduced to `Control.Monad` right away.

2. Do the switch.