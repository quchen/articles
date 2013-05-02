Haskell 2014: `Applicative => Monad` proposal
=============================================

Haskell calls a couple of historical accidents its own. While some of them, such as the "number classes" hierarchy, can be justified using arguments of practicality, there is one thing that stands out as, well, not that: `Applicative` not being a superclass of `Monad`.

A rough table of contents of the following text:

1. The general idea
2. Why that idea is good
3. A precise list of the proposed changes
4. Issues explcitly *not* covered by this proposal



The general idea
----------------

```haskell
class Applicative m => Monad m where
```

Short and simple! Let's do it! :-)



Consequences
------------

### Redundant and needlessly restricted functions

The proposed change renders a large amount of functions redundant. This fact also showcases the amount of bloating we currently have as a consequence.

- `pure` and `return` do the same thing. `Monad` will therefore not require the `return` function anymore in its typeclass declaration. Which of the two names `Applicative` should use remains open for debate.
- `liftM` and `liftA` can be removed due to now *being* `fmap`. The `liftM*` family is redundant because of `liftA*`, `<*>` is `ap`.
- Prelude's `sequence` requres `Monad` right now, while `Applicative` is sufficient to implement it. The more general version of this issue is captured by `Data.Traversable`, whose main typeclass implements the *same* functionality twice, namely `traverse` and `mapM`, and `sequenceA` and `sequence`.



### If it can be done, someone will do it

There will be no way of defining `Monad`s that do not have an `Applicative` instance anymore



### Easier type signatures

What's the type of `\f m -> fmap f m >>= return`? Well, it starts with requiring both `Monad` and `Functor`. I personally cringe every time I have to use `liftM` instead of `fmap` to make a function more (!) general.



### Beginner friendlier

How often did you say ...

- "A `Monad` is always an `Applicative` but due to historical reasons it's not but you can easily verify it by setting `pure = return` and `(<*>) = ap`"
- "`liftM` is `fmap` but not really." - "so when should I use `fmap` and when `liftM`" - *sigh*

Having the proper hierarchy will not answer these questions by design to the point where they cannot even come up anymore.



List of proposed changes
------------------------

1. Rename `Applicative`'s `pure` to `return`?

2. Remove `return` from the `Monad` typeclass, as `Applicative` provides this functionality already.

3. Add `join` to the `Monad` typeclass, with default implementation in terms of `>>=`. This is the more mathematical approach to a monad, and can be implemented more naturally in some cases.

4. Export `Applicative` from the Prelude.

5. Remove all functions rendered redundant by this proposal.

6. Add a legacy module to Base that re-defines the functions previously removed.




What this proposal is *not*
---------------------------

To keep this proposal concise, we should not include any semi-related issues in its discussion. For example, (some)one might want to move `fail` from `Monad` to a sub-class, rename `fmap` to `map` etc. These should get their own discussions if they are necessary.
