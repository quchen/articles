# My favorite Haskell function

My favorite Haskell function is `zipWith const`. It’s tiny. It’s in Prelude.
It’s awesome.

Although initially it may look like one of those random combinations of values
that typecheck but don’t do anything useful (`(>>=) (>>=)`, anyone?), it
encapsulates a powerful pattern that I found useful many times to make lists
useful as data structures. Being aware of how it works made me find a lot of use
cases, and I think everyone should know about this little peal!

A note on my frequent mentions of »works on infinite lists«. This is not just
about working with infinite data, which I agree is not that often the case.
Rather, it also means that the algorithm only consumes as much input as is
required, so it does not do any redundant work. This is of course especially
important, but not limited to, skipping an infinite amount of redundant work.

## Example 1: dropFromEnd

`drop` removes elements from the beginning of a list. But how do we remove
elements from the end?

The naive approach would be

```haskell
dropFromEnd n xs = take (length xs - n) xs
```

This does not work on infinite lists (where of course dropping n elements from
the non-existent end does not change the list at all).

Similarly,

```haskell
dropFromEnd n = reverse . drop n . reverse
```

traverses the list three times even, and also does not work for infinite inputs.

At this point, you may be wondering how I’ll bridge the gap to selling you a
cool solution with `zipWith const`, so here we go:

```haskell
dropFromEnd n xs = zipWith const xs (drop n xs)
```

Wait, how does that work? Well, `zipWith const xs` has the same entries as `xs`,
but since `zipWith` stops when one of its arguments has been fully traversed, it
will only have as many entries as `drop n xs`. So, we get the list `xs`, but
with `n` elements dropped from the end.

## Example 2: Split list in half

To split a list in the middle, the naive solution is

```haskell
splitMiddle xs = splitAt (length xs `div` 2) xs
```

Like `dropFromEnd`, this produces nothing for infinite input. But we can do
better, using a similar trick as before. All we have to do is produce a list
that has half the length of the input list, and use `zipWith const` again. We
can do this by dropping every other element:

```haskell
halfAsLong (x:_:xs) = x : halfAsLong xs
halfAsLong _ = []
```

Using this, we can generate the first half:

```haskell
firstHalf xs = zipWith const xs (halfAsLong xs)
```

For the second half, we can now drop the first firstHalf-many elements from the
input list,

```haskell
zipOverflow (_:xs) (_:ys) = zipOverflow xs ys
zipOverflow [] ys = ys
zipOverflow xs [] = xs
```

Putting this together, we get

```haskell
splitMiddle xs = let firstHalf = zipWith const xs (halfAsLong xs)
                     secondHalf = zipOverflow firstHalf xs
                 in (firstHalf, secondHalf)
```

This works even for infinite lists, where the input will be in the first
component of the tuple, and the second one is of course bottom.

## Example 3: rotateWhile

One last example that I recently needed. Backstory: a polygon can be described
by the ordered list of its corners. But it’s not actually important which corner
is »first«, as long as each corner has the same corner after it, the two
polygons are equivalent. But if we naively compare the list of corners, we get
the standard list comparison, which does not respect the rotational equivalence
of polygons.

As a solution, we should write the equality instance

```haskell
newtype Polygon = Polygon [(Double, Double)]
instance Eq Polygon where
    Polygon xs == Polygon ys = normalize xs == normalize ys
instance Ord Polygon where
    compare (Polygon xs) (Polygon ys) = compare (normalize xs) (normalize ys)
```

where `normalize` rotates the corner lists to have some arbitrary choice of
element as its first, such as the minimum of the contained points. It doesn’t
matter what »minimum« actually means for points, it’s just important that there
is a well-behaved ordering, which luckily, tuples do have (compare `fst`, and on
tie, compare `snd`).

Like last time, we can create a list of appropriate elements, and a list of
desired length, use `zipWith const`, and get our result:

```haskell
rotateUntil :: (a -> Bool) -> [a] -> [a]
rotateUntil p xs = zipWith
    (flip const)
    xs
    (dropWhile (not . p) (cycle xs))
```

And thus we get

```haskell
normalize (Polygon xs) = rotateUntil (== minimum xs) xs
```

## Notes.

- In the last example I’m using `==` on Doubles. That’s fine. `+` is the worse
  offender, but nobody yells at you when you use that one. Actually, up to
  `NaN`, equality of Doubles is perfectly well-behaved.
