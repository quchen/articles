Class law based rewrite rules
=============================

Currently class laws are not used for rewriting code. The reason for this is simple: current "laws" should more accurately be called "recommendations", therefore the compiler cannot rely on them. This is a proposal to make use of these recommendations.



The idea
--------

Classes should be able to come with their own version of `RULES` pragmas, call them `LAW`, like so:

```haskell
class Functor f where
      fmap :: (a -> b) -> f a -> f b
      {-# LAW "fmap/id"
            forall (x :: Functor f => f a).
            fmap id x = x
            #-}
      {-# LAW "fmap/fuse"
            forall (f :: b -> c)
                   (g :: a -> b).
            fmap f . fmap g = fmap (f . g)
            #-}
```

However, `LAW`-based rewrite rules are only applied for instances that explicitly obey them:

```haskell
instance Maybe Functor where
      fmap _ Nothing = Nothing
      fmap f (Just x) = Just (f x)
      {-# LAW-OBEDIENT-INSTANCE #-} -- Apply LAW rules to this instance


data Counter a = Counter Int a
instance Functor Counter where
      fmap f (Counter c x) = Counter (c+1) (f x)
      -- Not law obedient, don't apply rewrites
```

Another thing to consider is rewriting non-class functions to make use of this idea. For example

```haskell
ap mf mx = do { f <- mf; x <- mx; return (f x) }
{-# LAW-OBEDIENT m "ap/<*>"
      forall (mf :: Monad m => m (a -> b))
             (mx :: Monad m => m a).
      ap mf mx = (<*>) mf mx
      #-}
```

indicates that this rule should only apply when the instance for `m` is marked as obedient.



Benefits
--------

- Applicative `do` notation for free: due to the new rewrite rules, obedient instances can be rewritten to Applicative style automatically (and this similarly applies to all other class hierarchies).

- Laws marked as the pragma should simplify code in the same manner as `RULES` do, which can be good for performance. Obeying a law that makes things more complicated should therefore not be mentioned in a `LAW` pragma.




Problems
--------

- Some instances obey only a subset of the laws. The proposal above skips treating those separately in order to make the instance implementation simple, as opposed to adding fine-grained control over which laws are applicable.

- What to do in the presence of ⊥? For example `Reader` doesn't satisfy `⊥ >>= return = ⊥`. Should `LAW` apply?

