# Functions vs. lambdas: subtle difference with huge impact

## tl;dr

`f x =` and `f = \x ->` are sometimes much more than a stylistic difference with
potentially huge performance gains due to sharing, like here:

```haskell
f x y = … expensive x …

-- vs

f x = let cached = … expensive x …
      in \y -> … cached …
```

## Detailed example

Suppose you have a very expensive function to calculate, but you don’t need to
do it super accurately, for example the [Gamma function][gamma], or in my case:
the sum of thousands of [Coulomb forces][coulomb] for a graphics/physics
project. Pre-calcualting the values once into a lookup table and then doing
calculations on interpolations of that lookup table can yield quite the speedup.

Assume we have these from a library somewhere,

```haskell
-- Create a lookup table for a function within a certain range of coordinates
createLookupTable
    :: RangeOf Coordinate
    -> (Coordinate -> Value)
    -> LookupTable

-- Look up a value in a table (possibly doing smart interpolation)
lookup
    :: LookupTable
    -> Coordinate
    -> Value

-- This is what we want!
interpolated
    :: RangeOf Coordinate
    -> (Coordinate -> Value)
    ->  Coordinate -> Value
```


### Bad: naive version

Then code for calculating function values might look like this:

```haskell
interpolated region f x =
    let lut = createLookupTable region f
    in lookup lut x
```

This is of course super inefficient, because for any single value `x`, we
calculate the whole lookup table. Not doing the lookup table at all would be
better, so why even bother with one!

### Better: cache values a priori

We should pass at least a number of values so we can shareit.

```haskell
interpolated region f xs =
    let lut = createLookupTable region f
    in Map.fromSet (lookup lut) xs
```

This does what we want, but now we have to worry about what values we’re going
to need later, which is also not ideal. And the type is also harder to read now,

```haskell
interpolated
    :: RangeOf Coordinate
    -> (Coordinate -> Value)
    -> Set Coordinate
    -> Map Coordinate Value
```

### Best: only share the lookup table!

Let’s ditch the `Map`/`Set` again and look at the naive version again; we notice
that we don’t actually need the `x` in the creation of the lookup table. Can we
move »taking `x` as an argument« after the creation of the lookup table? We can,
by returning a lambda!

```haskell
interpolated region f =
    let lut = createLookupTable region f
    in \x -> lookup lut x
```

We can now use this function like so:

```haskell
let fCached = interpolated region f
in map fCached manyValues
```

This will implicitly share the lookup table for all invocations of `fCached`,
because it, is closed into it. Garbage collection will remove the lookup table
along with `fCached` when no longer needed. The best of the worlds!


[gamma]: https://en.wikipedia.org/wiki/Gamma_function
[coulomb]: https://en.wikipedia.org/wiki/Coulomb%27s_law
