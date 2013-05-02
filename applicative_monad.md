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

2. Add `Applicative` to the Report, and export it from the Prelude.

3. Add `join` to the `Monad` typeclass, with default implementation in terms of `>>=`. This is the more mathematical approach to a monad, and can be implemented more naturally than bind in some cases.



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

Code defining monads without giving an `Applicative` instance will blow up. Not sure what to say about this.



Code outline
------------

The following code shows the changes to the class hierarchy proposed in one possible form:

```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b



class Functor f => Applicative f where

    pure :: a -> f a

    (<*>) :: f (a -> b) -> f a -> f b

    (*>) :: f a -> f b -> f b
    (*>) = liftA2 (const id)

    (<*) :: f a -> f b -> f a
    (<*) = liftA2 const



class Applicative m => Monad m where

    return :: a -> m a
    return = pure

    (>>=) :: m a -> (a -> m b) -> m b
    m >>= f = join $ fmap f m

    (>>) :: m a -> m b -> m b
    (>>) = (*>)

    join :: m (m a) -> m a
    join m = m >>= id

    fail :: String -> m a
    fail = error

-- Useful as a default implementation for fmap
liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM f m = m >>= return . f

-- Same as liftA2 except for an explicitly monadic constraint
liftM2 :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
liftM2 = liftA2

-- etc
```