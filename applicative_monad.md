Haskell 2014: `Applicative => Monad` proposal
=============================================

Haskell calls a couple of historical accidents its own. While some of them, such as the "number classes" hierarchy, can be justified using arguments of practicality, there is one thing that stands out as, well, not that: `Applicative` not being a superclass of `Monad`.

A rough table of contents of the following text:

1. The general idea
2. Why that idea is good
3. A precise list of the proposed changes
4. Issues explcitly *not* covered by this proposal
5. Code example of the new hierarchy



The general idea
----------------

```haskell
class Applicative m => Monad m where
```

Short and simple! Let's do it! ;-)



Discussion of the consequences
------------------------------

### Redundant functions

The proposed change renders a large amount of functions redundant. This fact also showcases the amount of bloating we currently have.

- `pure` and `return` do the same thing.
- `>>` and `*>` are identical.
- `liftM` and `liftA` are `fmap`. The `liftM*` is `liftA*`, `<*>` is `ap`.
- Prelude's `sequence` requres `Monad` right now, while `Applicative` is sufficient to implement it. The more general version of this issue is captured by `Data.Traversable`, whose main typeclass implements the *same* functionality twice, namely `traverse` and `mapM`, and `sequenceA` and `sequence`.



### If it can be done, someone will do it

There will be no way of defining `Monad`s that do not have a `Functor`/`Applicative` instance anymore: if you can use `>>=`, you can use `fmap`.

Additionally, what's the type of `\f m -> fmap f m >>= return`? Well, it starts with `(Monad m, Functor m)`. I personally cringe every time I have to use `liftM` instead of `fmap` to make a function more (!) general.



### Beginner friendliness

How often did you say ...

- "A `Monad` is always an `Applicative` but due to historical reasons it's not but you can easily verify it by setting `pure = return` and `(<*>) = ap`"
- "`liftM` is `fmap` but not really." - "So when should I use `fmap` and when `liftM`?" - *sigh*

With the new hierarchy, the answer would be "use the least restrictive one and you won't run into any issues".



### Compatibility issues

???



List of proposed changes
------------------------

(Note that these can - and should be - be decided individually.)

1. Rename `Applicative`'s `pure` to `return`, and remove `return` from `Monad`.

2. Add `join` to the `Monad` typeclass, with default implementation in terms of `>>=`. This is the more mathematical approach to a monad, and can be implemented more naturally than bind in some cases.

3. Export `Applicative` from the Prelude.

4. Change functions that are currently monadic to using `Applicative` when possible (examples: `sequence`, `mapM`).

Some of these may seem rather radical, so let me explain my rationale. This is not merely a "fix" of Base - it is supposed to change the *language standard*. Its consequences will define the language for many years. For this reason, it should not simply implement the minimal changes to make the idea work, but instead be a consistent definition of it. On the contrary, the introduction of a legacy module makes this change possible with minimal maintenance for fixing existing libraries.

The following things should not change:

1. Functions rendered redundant by the new hierarchy may still have ther places. For example, `liftM` is a valid definition of `fmap` if you want to get a cheap `Functor` instance from an already defined `Monad`.

2. Some functions have too restrictive types. For example, `liftA2` and `liftM2` do the same thing, but the explicitly monadic version should have an appropriate constraint. However, due to the fact that the only difference is a type specialization, `liftM* = liftA*` would be a valid implementation. Keeping this serves two purposes: most importantly, it maintains compatibility; second, some functions may be preferrable in a monadic setting, such as `>>`, which is technically the same as `*>`, which is more used in Applicative style.



What this proposal is not
-------------------------

To keep this proposal concise, we should not include any semi-related issues in its discussion. For example, (some)one might want to move `fail` from `Monad` to a sub-class, rename `fmap` to `map` etc. These should get their own discussions if someone feels they are necessary.



Code outline
------------

The following code shows the changes to the class hierarchy proposed in one possible form:

```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b



class Functor f => Applicative f where
    return :: a -> f a

    (<*>) :: f (a -> b) -> f a -> f b

    (*>) :: f a -> f b -> f b
    (*>) = liftA2 (const id)

    (<*) :: f a -> f b -> f a
    (<*) = liftA2 const



class Applicative m => Monad m where
    (>>=) :: m a -> (a -> m b) -> m b
    m >>= f = join $ fmap f m

    (>>) :: m a -> m b -> m b
    (>>) = (*>)

    join :: m (m a) -> m a
    join mma = mma >>= id

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