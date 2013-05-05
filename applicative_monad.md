Haskell 2014: `Applicative => Monad` proposal
=============================================

Haskell calls a couple of historical accidents its own. While some of them, such as the "number classes" hierarchy, can be justified using arguments of practicality, there is one thing that stands out as, well, not that: `Applicative` not being a superclass of `Monad`.



The general idea
----------------

```haskell
class Applicative m => Monad m where
```

The goal of this proposal is maintaining compatibility at all possible cost. Maybe in the long run overcoming this initial hurdle allows changing other functions to require only `Applicative` instead of `Monad` (think of `sequence`), but this is beyond the current scope.



List of proposed changes
------------------------

1. Make `Applicative` a superclass of `Monad`.

2. Add `Applicative` to the Report, define it in the `Prelude`, and re-export it from `Control.Applicative`.

3. Add `join` to the `Monad` typeclass, with default implementation in terms of `>>=`. This is the more mathematical approach to a monad, and can be implemented more naturally than bind in some cases. Remove and re-export it from `Control.Monad` (so that qualified uses don't break).



Discussion of the consequences
------------------------------



### Redundant functions

- `pure` and `return` do the same thing.
- `>>` and `*>` are identical.
- `liftM` and `liftA` are `fmap`. The `liftM*` is `liftA*`, `<*>` is `ap`.
- Prelude's `sequence` requres `Monad` right now, while `Applicative` is sufficient to implement it. The more general version of this issue is captured by `Data.Traversable`, whose main typeclass implements the *same* functionality twice, namely `traverse` and `mapM`, and `sequenceA` and `sequence`.

While it is not proposed to remove the redundancies, the change would make it the programmer's option to use them, instead of having the type system forcing the use of one of seemingly identical functions, as is the case right now.
(What's the type of `\f m -> fmap f m >>= return`? Well, it starts with `(Monad m, Functor m)`. I personally cringe every time I have to use `liftM` instead of `fmap` to make a function more general.)



### Beginner friendliness

How often did you say ...

- "A `Monad` is always an `Applicative` but due to historical reasons it's not but you can easily verify it by setting `pure = return` and `(<*>) = ap`"
- "`liftM` is `fmap` but not really." - "So when should I use `fmap` and when `liftM`?" - *sigh*

With the new hierarchy, the answer would be "use the least restrictive one and you won't run into any issues".



### If it can be done, someone will do it

There will be no way of defining a `Monad` that does not not have a `Functor`/`Applicative` instance anymore: if you can use `>>=`, you can use `fmap`.



### Compatibility issues

Compatibility issues *with non-GHC libraries* can be categorized as follows:

1. Monads lacking `Functor` or `Applicative` instances. This is easily fixable by either setting `fmap = liftM`, `pure = return` and `(<*>) = ap`, although more efficient implementations may exist, or by moving an already existing definition from `Control.Applicative` to the appropriate module.

2. This one is specific to building GHC: importing `Control.Monad/Applicative` introduces a circular module dependence. In this case, one can rely on handwritten implementations of the desired function, e.g. `ap f x = f >>= ...`.

3. Libraries using their own `(<*>)`. This one is much tougher, as renaming the operator may require a lot of effort. For building GHC though, this only concerns Hoopl, and a handful of renames.




How to apply this change
------------------------

The first t

1. **Preparing GHC for the change.** Apply the full `Applicative => Monad` change to GHC's code and fix the emerging compilation errors by giving all `Monads` `Applicative` and `Functor` instances. Once the build works, revert the change, but leave the instance definitions in. Note that this does not actually change anything about Haskell or GHC in practice, it is purely internal.

2. **Preparing Hackage for the change.** Using a version of GHC with `Applicative => Monad` built in, compile as many Hackage libraries as possible. This should give us an overview of how large the proposed change actually is in practice. For modules that break, email the maintainer about the issue, and hope it's fixable.

3. **Haskell' proposal.** This is not primarily a GHC, but a Haskell change. The previous steps were basically preparing the landscape for the change, and when we've (hopefully) found out that it is a good idea to go through with it, it can be proposed to go into the report.