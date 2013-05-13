Haskell 2014: `Applicative => Monad` proposal
=============================================

Haskell calls a couple of historical accidents its own. While some of them, such as the "number classes" hierarchy, can be justified by pragmatism, there is one thing that stands out as, well, not that: `Applicative` not being a superclass of `Monad`.

I will use the abbreviation *AMP* for the "`Applicative => Monad` Proposal".

Table of contents
-----------------

1. The general idea
1. List of proposed changes
1. Discussion of the consequences
1. How to apply this change



The general idea
----------------

```haskell
class Applicative m => Monad m where
```

The rationale behind this proposal's contents is as follows:

1. **Break as little code as possible.** For example, don't move `return` to `Applicative` and remove `pure`. Instead, leave `return` in `Monad`, and give it `pure` as default implementation.

2. **Change only things that are closely related to the proposal.** For example, using `join` in a monad definition requires it to be a functor, so it goes hand in hand with the AMP. On the other hand, removing `fail` has nothing to do with what we're trying to accomplish.






List of proposed changes
------------------------

1. Make `Applicative` a superclass of `Monad`.

2. Add `Applicative` to the Report, define it in the `Prelude`, and re-export it from `Control.Applicative` for compatibility. Similar to `Functor` and `Monad`, only the most basic functions should be accessible in the `Prelude` without further imports, so that'll probably be only the functions defined by the typeclass.

3. Add `join` to the `Monad` typeclass, with default implementation in terms of `>>=`. This is the more mathematical approach to a monad, and can be implemented more naturally than bind in some cases (e.g. List and Reader). Remove and re-export it from `Control.Monad` (so that qualified uses don't break). (This has previously been impossible because of the fact that it requires a `Functor` instance to make `>>=` work out of the box.)

4. (Proposed in #haskell) Add `Alternative => MonadPlus`.



Discussion of the consequences
------------------------------



### It's the right thing to do™ :-)

Math etc. You've all heard this one, it's good and compelling so I don't need to spell it out. Moving on,



### Performance :-)

Using `Applicative` can be beneficial to performance, as the code can potentially be optimized better.

An example: a `State` computation with `Applicative` either always or never uses `put`; whether it does can only depend on external parameters, and not on the intermediate results. On the other hand, a monadic computation can depend on previous results, so a `State` `Monad` can, but does not always have to, use `put`.

With the AMP, monadic computations (especially `do` blocks used for their readability) could be rewritten to use `Applicative` functions when possible.



### Redundant functions :-)

- `pure` and `return` do the same thing.
- `>>` and `*>` are identical.
- `liftM` and `liftA` are `fmap`. The `liftM*` are `liftA*`, `<*>` is `ap`.
- Prelude's `sequence` requres `Monad` right now, while `Applicative` is sufficient to implement it. The more general version of this issue is captured by `Data.Traversable`, whose main typeclass implements the *same* functionality twice, namely `traverse` and `mapM`, and `sequenceA` and `sequence`.

That very much violates the "don't repeat yourself" principle, and even more so it forces the programmer to repeat himself to achieve maximal generality. It may be too late to take all redundancies out, but at least we can prevent new ones from being created.



### Compatibility issues :-(

These are the kinds of issues to be expected:

1. Monads lacking `Functor` or `Applicative` instances. This is easily fixable by either setting `fmap = liftM`, `pure = return` and `(<*>) = ap`, although more efficient implementations may exist, or by moving an already existing definition from `Control.Applicative` to the appropriate module.

2. This one is specific to building GHC: importing `Control.Monad/Applicative` introduces a circular module dependence. In this case, one can rely on handwritten implementations of the desired function, e.g. `ap f x = f >>= ...`.

3. Libraries using their own `(<*>)`. This one is much tougher, as renaming the operator may require a lot of effort. For building GHC though, this only concerns Hoopl, and a handful of renames.




### Beginner friendliness :-)

How often did you say ...

- "A `Monad` is always an `Applicative` but due to historical reasons it's not but you can easily verify it by setting `pure = return` and `(<*>) = ap`"
- "`liftM` is `fmap` but not really." - "So when should I use `fmap` and when `liftM`?" - *sigh*

With the new hierarchy, the answer would be "use the least restrictive one and you won't run into any issues".



### If it can be done, someone will do it :-)

There will be no way of defining a `Monad` that does not not have a `Functor`/`Applicative` instance anymore: if you can use `>>=`, you can use `fmap`.




How to apply this change
------------------------

1. **Preparing GHC.** Using a GHC fork with the full patch applied, find and fix all compilation errors introduced by the change by adding `Functor`/`Applicative` instances for all `Monads`. *This should be done regardless of whether the AMP actually makes it.*

2. **Preparing Hackage.** Using the same modified compiler as above, compile as many libraries on Hackage as possible to see how many packages break. This should give us an overview of how large the proposed change actually is in practice. Send an email or a patch to the maintainer to fix the issue. *This should also be done regardless of whether the AMP actually makes it.*

3. **Haskell Prime proposal.** This is not primarily a GHC, but a Haskell change. The previous steps were basically preparing the landscape for the change, and when we've (hopefully) found out that it is a good idea to go through with it, it can be proposed to go into the Report.




Status report
-------------

- Todo: Everything.