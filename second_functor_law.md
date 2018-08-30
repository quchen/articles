The second `Functor` law is redundant
=====================================

Basic Haskell teaches that instances of `Functor` should satisfy

```haskell
fmap id = id                   -- fmap preserves identity
fmap f . fmap g = fmap (f . g) -- fmap distributes over composition
```

However, Haskell's type system is special in a way that makes the second law
follow from the first one. The values of *every* type follow a certain law;
this law is called the free theorem of that type. For example, the free theorem
of `f :: [a] -> [a]` is

```haskell
map g . f  = f . map g
```

This means that any function `f` of type `[a] -> [a]` commutes with `map`;
for example one can choose `f = reverse`, and then the law reads

```haskell
map g . reverse = reverse . map g
```

This holds for all `g`; so from the type of `f` alone, one can deduce that
we can rearrange it in a chain of `map`s to be in any place we want.
Pretty cool how something like that can be derived from a type alone!

Similarly, you can generate the free theorem for `fmap`, which reads

```haskell
        f .      g =      p .      q -- (1) Given this ...
=> fmap f . fmap g = fmap p . fmap q -- (2) ... this holds
```

In other words, this says that whenever functions compose, fmapping all of them
still composes.

Now choose `p = id` and `q = f . g`. (1) clearly holds in this case, so we can
derive

```haskell
  fmap f . fmap g
= fmap id . fmap (f . g)
= id . fmap (f . g)      -- by the first functor law
= fmap (f . g)
```

This is precisely the second Functor law,

```haskell
fmap f . fmap g = fmap (f . g)
```

Note how we used nothing but `fmap`'s type (to generate the free theorem) and
the first `Functor` law (to eliminate `fmap id`) to derive this.



## Bottom ruins the party

[It is worth mentioning that this only holds up to fast and loose reasoning,
i.e. assuming no ⊥ are involved][fastandloose], otherwise e.g.

```haskell
newtype Id a = Id a

instance Functor Id where
      fmap f x = f `seq` x `seq` (Id . f . runId) x
```

satisfies the first, but not the second, Functor law:

```haskell
fmap id x = id `seq` x `seq` (Id . id . runId) x
          = id `seq` (x `seq` (Id . id . runId) x) -- seq is infixr 0
          = x `seq` (Id . runId) x
          = x `seq` x
          = x

-- but
(fmap (const ()) . fmap ⊥) x
      = fmap (const ()) (fmap ⊥ x)
      = fmap (const ()) (⊥ `seq` <stuff>)
      = fmap (const ()) ⊥
      = <stuff> `seq` ⊥ `seq` ...
      = ⊥
fmap (const () . ⊥) x
      = (const () . ⊥) `seq` x `seq` (Id . (const ()) . ⊥) . runId) x
      = const () . ⊥ `seq` (x `seq` (Id . const () . ⊥ . runId) x)
      = x `seq` (Id . const () . ⊥ . runId) x
      = x `seq` Id (const () (⊥ (runId x)))
      = x `seq` Id ()
      -- This is ⊥ if and only if x is ⊥.
```

## The converse: 2nd law holds, 1st does not

The following hypothetical `Functor Maybe` satisfies the second, but not the
first, Functor law:

```haskell
instance Functor Maybe where
    fmap _ _ = Nothing
```
```haskell
-- 1st law broken
fmap id (Just ()) = Nothing

-- 2nd law holds
fmap f . fmap g = const Nothing . const Nothing = const Nothing
fmap (f . g) = const Nothing
```



## Further reading

If you want to know more about free theorems or just play around with them:

- There is also an [article by Edward Kmett on free theorems][kmett-free-fmap]
  that you might like.
- [Online free theorem generator][ftgen] (make sure to "hide type
  instantiations"; the free theorem is displayed in the very last box in a
  somewhat readable format)
- [Theorems for free!][tff], the original publication on free theorems

[fastandloose]: http://www.cse.chalmers.se/~nad/publications/danielsson-et-al-popl2006.html
[ftgen]: http://www-ps.iai.uni-bonn.de/cgi-bin/free-theorems-webui.cgi
[kmett-free-fmap]: https://www.fpcomplete.com/user/edwardk/snippets/fmap
[tff]: http://homepages.inf.ed.ac.uk/wadler/papers/free/free.ps
