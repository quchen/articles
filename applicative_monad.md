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

Short and simple! Let's do it! ;-)



Discussion of the consequences
------------------------------

### Redundant functions

The proposed change renders a large amount of functions redundant. This fact also showcases the amount of bloating we currently have as a consequence.

- `pure` and `return` do the same thing. `Monad` will therefore not require the `return` function anymore in its typeclass declaration. Which of the two names `Applicative` should use remains open for debate.
- `>>` and `*>` are identical.
- `liftM` and `liftA` can be removed due to now *being* `fmap`. The `liftM*` family is redundant because of `liftA*`, `<*>` is `ap`.
- Prelude's `sequence` requres `Monad` right now, while `Applicative` is sufficient to implement it. The more general version of this issue is captured by `Data.Traversable`, whose main typeclass implements the *same* functionality twice, namely `traverse` and `mapM`, and `sequenceA` and `sequence`.



### Compatibility issues

Removing functions previously defined will break many (more realistically: probably all) modules. Therefore, a compatibility "legacy" module containing the now redundant functions (`liftM` and friends), should re-define all these functions.



### If it can be done, someone will do it

There will be no way of defining `Monad`s that do not have a `Functor`/`Applicative` instance anymore: if you can use `>>=`, you can use `fmap`.

Additionally, what's the type of `\f m -> fmap f m >>= return`? Well, it starts with `(Monad m, Functor m)`. I personally cringe every time I have to use `liftM` instead of `fmap` to make a function more (!) general.



### Beginner friendliness

How often did you say ...

- "A `Monad` is always an `Applicative` but due to historical reasons it's not but you can easily verify it by setting `pure = return` and `(<*>) = ap`"
- "`liftM` is `fmap` but not really." - "so when should I use `fmap` and when `liftM`?" - *sigh*

Having the proper hierarchy will not answer these questions *by design* to the point where they cannot even come up anymore.



List of proposed changes
------------------------

One thing to particularly stress here is that this is not merely a "fix" of an issue - it is supposed to change the language standard (i.e. the Report). Its consequences will define the language for many years. For this reason, it should not simply implement the minimal changes to make the idea work, but instead be a consistent definition of the idea.

1. Rename `Applicative`'s `pure` to `return`?

2. Remove `return` from the `Monad` typeclass, as `Applicative` provides this functionality already.

3. Add `join` to the `Monad` typeclass, with default implementation in terms of `>>=`. This is the more mathematical approach to a monad, and can be implemented more naturally in some cases.

4. Export `Applicative` from the Prelude.

5. Remove all functions rendered redundant by this proposal. (Since this concerns the Report, deprecation is not an option.) Exception: due to their use in different programming styles, `>>` and `*>` should both be kept. However, `>>` could be removed from the typeclass to being an ordinary top-level synonym for `*>`.

6. Add a legacy module to Base that re-defines the functions previously removed.




What this proposal is *not*
---------------------------

To keep this proposal concise, we should not include any semi-related issues in its discussion. For example, (some)one might want to move `fail` from `Monad` to a sub-class, rename `fmap` to `map` etc. These should get their own discussions if they are necessary.



Code outline
------------

The following code shows the changes to the class hierarchy proposed in one possible form:

```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b



class Functor f => Applicative f where
    return :: a -> f a

    (<*>)  :: f (a -> b) -> f a -> f b

    (*>)   :: f a -> f b -> f b
    (*>) = liftA2 (const id)

    (<*)   :: f a -> f b -> f a
    (<*) = liftA2 const



class Applicative m => Monad m where
    (>>=) :: m a -> (a -> m b) -> m b
    m >>= f = join $ fmap m f

    join :: m (m a) -> m a
    join mma = mma >>= id

    fail :: String -> m a


-- Note the Monad constraint!
(>>) :: Monad m => m a -> m b -> m b
(>>) = (*>)
```