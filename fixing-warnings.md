Locally hiding warnings
=======================

GHC does currently not support selectively hiding warnings for single lines or
blocks of code. But sometimes the warning is unjustified, and we would like to
disable it for some piece of code.





Unused values
-------------

### tl;dr

```haskell
_ = unusedDefinition
```

### When is this useful?

With `-W-unused-binds`, GHC warns when a non-exported value is never used. But
there are valid cases of having this, such as when names of values are
implicitly used by Template Haskell. Consier the following code:

```haskell
data Entry k v = Entry { key :: k, value :: v }

$(deriveJSON defaultOptions ''Entry)
```

This might serialize an entry to `{ "key": X, "value": Y }`, because the record
fields have names. Deleting the unused `key` and `value` fields leads to the
different serialization `[X, Y]`. If we want to have the explicit field names,
we're left with unused binings GHC warns us about.

### Disabling the warning

The Haskell Report says

> Compilers that offer warnings for unused identifiers are encouraged to
> suppress such warnings for identifiers beginning with underscore.

We can use that to silence warnings for unused bindings, by simply introducing
a synonym for our unused value, and binding it to a name beginning with `_`:

```haskell
_ = (key, value)
```

GHC will now mark `key` and `value` as used by the binding `_`, and since `_`
does not generate any unused warnings, neither will `key` and `value`. A
comment as to why this definition might be useful is probably a good idea here.

Instead of a comment, we might want to give the unused pattern a better name,
such as `_unused`, but then GHC will warn about a missing top-level type
signature, so I think it's better to just leave it at `_` and a short comment.





Incomplete patterns
-------------------

### tl;dr

Add a default pattern with a desciptive call to `error` if the branch is ever
reached.

### When is this useful?

Consider the code

```haskell
[a, b, c] = map f [x, y, z]
```

As a programmer, we know that the pattern will always match, but the compiler
cannot decide this in general, hence it will warn us that we only handle lists
of length 3.

### Disabling the warning

We'll have to handle all possible branches manually in order to disable the
warning, and use dummy values for unreachable branches. It's a good idea to
make these dummies as descriptive as possible, for if the code changes or we
make a mistake, we find the offending place as easily as possible.

```haskell
(a,b,c) = case map f [x,y,z] of
    [x', y', z'] -> (x', y', z')
    _otherwise   -> error "`map` changed the length of the list! X.hs:12"
```





Missing type signatures
-----------------------

Add a type signature. :-P





Orphan instances
----------------

Put all orphan instances in `.Orphans` modules, and disabling orphan warnings
via `{-# OPTIONS_GHC -fno-warn-orphans #-}`. These modules should contain
nothing but the instance definitions, thus have empty export lists. This makes
it very clear that the programmer knows what orphans are and that warnings for
them have been explicitly disabled.
