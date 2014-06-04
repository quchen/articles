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
        p .      q =      f .      g -- (1) Given this ...
=> fmap p . fmap q = fmap f . fmap g -- (2) ... this holds
```

In other words, this says that whenever functions compose, fmapping all of them
still composes.

We also have another trivial law,

```haskell
         p . q  =       f . g  -- Given this (same as above) ...
=> fmap (p . q) = fmap (f . g) -- (3) ... we can just apply fmap on
                               -- both sides; referential transparency
                               -- guarantees both sides stay equal
```

Now let's choose `p = id`, and we'll get

```haskell
fmap id . fmap q = fmap f . fmap g -- (2) specialized
fmap (id . q) = fmap (f . g)       -- (3) specialized
```

Using the first `Functor` law, we can evaluate the first line, and in the
second line `q` is simply composed with the identity. So evaluating both of
these one step yields

```haskell
fmap q = fmap f . fmap g
fmap q = fmap (f . g)
```

Since the left hand sides of both lines are equal so are their right hand sides,
yielding

```haskell
fmap f . fmap g = fmap (f . g)
```

This is precisely the second Functor law. Note how we used nothing but `fmap`'s
type (to generate the free theorem) and the first `Functor` law (to eliminate
`fmap id`) to derive this.

If you want to know more about free theorems or just play around with them:

- [Online free theorem generator][ftgen] (make sure to "hide type
  instantiations"; the free theorem is displayed in the very last box in a
  somewhat readable format)
- [Theorems for free!][tff], the original publication on free theorems

[ftgen]: http://www-ps.iai.uni-bonn.de/cgi-bin/free-theorems-webui.cgi
[tff]: http://homepages.inf.ed.ac.uk/wadler/papers/free/free.ps