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

Now choose `p = id` and `q = f . g` in (2),

``haskell
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
the first `Functor` law (to eliminate `fmap id`) to derive this. [It is worth
mentioning that this only holds up to fast and loose reasoning, i.e. assuming
no ⊥ are involved][fastandloose], otherwise e.g.
``fmap f x = f `seq` x `seq` (Identity . f . runIdentity) x`` satisfies the
first, but not the second, Functor law.

If you want to know more about free theorems or just play around with them:

- [Online free theorem generator][ftgen] (make sure to "hide type
  instantiations"; the free theorem is displayed in the very last box in a
  somewhat readable format)
- [Theorems for free!][tff], the original publication on free theorems

[ftgen]: http://www-ps.iai.uni-bonn.de/cgi-bin/free-theorems-webui.cgi
[tff]: http://homepages.inf.ed.ac.uk/wadler/papers/free/free.ps
[fastandloose]: http://www.cse.chalmers.se/~nad/publications/danielsson-et-al-popl2006.html
