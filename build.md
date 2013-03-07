The `build` function
==========================


Abstract
--------

The function `build` from `GHC.Exts` has a rather peculiar type, but it seems to appear quite often in basic modules like `Data.List`. This text is supposed to clarify how it works, and what it is useful for.

Super shrt version: `build` converts one list representation to another, and is useful as an intermediate code optimization function.



How does it work?
-----------------

The build function is defined as

```haskell
build :: forall a. (forall b. (a -> b -> b) -> b -> b) -> [a]
build g = g (:) []
```

Don't let the type scare you, the idea behind this function is actually quite simple: you're used to defining lists as

```haskell
data List a = Cons a (List a) | Nil

myList = 1 `Cons` (2 `Cons` (3 `Cons` Nil))
```

This encapsulates the idea "a list is either an element and then the rest of the list, or empty in the first place". However, there is another way of describing the same idea: a lambda that parameterizes over `Nil` and `Cons`, which is an example of functions as data in Haskell. I will call them lambda lists here:

```haskell
myLambdaList = \cons nil -> 1 `cons` (2 `cons` (3 `cons` nil))
```

This has its up- and downsides. The downside: it's a function, so you can't do all the things with it you can't do with functions, such as comparing, printing etc. The upside will be covered in the next section. For now just look at it as an oddity.

So if you *did* want to compare two lambda lists (or any other ordinary list operation), you would have to convert it back to an ordinary list, which you would of course do by inserting `(:)` for `cons`, and `[]` for `nil`:

```haskell
myOrdinaryList = myLambdaList (:) []
               = (\cons nil -> 1 `cons` (2 `cons` (3 `cons` nil))) (:) []
               = 1 : (2 : (3 : []))
               = [1,2,3]
```

Now if you look at the first line, that's exactly how `build` looks like, and that's exactly how it works: **`build` converts a lambda list to an ordinary list.**



What is it good for?
--------------------

One way of picturing how `foldr f z` works is that it takes a list, and replaces every occurrence of `(:)` with `f`, and `[]` with `z`. Sound familiar? That's the inverse of `build`! `build` inserted `nil` and `cons` in a function to produce a list, `foldr` takes a list and inserts `f` and `z`. So if you first use `build` to convert a lambda list to a normal list `foldr` can use, and immediately afterwards use `foldr` to deconstruct the list and replace the `[]` and `(:)` with a new function, why not omit generating the intermediate list? Just insert `f` for `(:)` and `z` for `[]` in the lambda list, and you'll get the same result. As a formula:

```haskell
foldr f z (build g)  ==  g f z
```

An example:

```haskell
-- Recall the definition:
myLambdaList = \cons nil -> 1 `cons` (2 `cons` (3 `cons` nil))

-- The short way: using the rule
foldr (+) 0 (build myLambdaList)
      = (\cons nil -> 1 `cons` (2 `cons` (3 `cons` nil))) (+) 0
      = 1 + (2 + (3 + 0))
      = 6

-- The long way: building an intermediate list
foldr (+) 0 (build myLambdaList)
      = foldr (+) 0 $ (\cons nil -> 1 `cons` (2 `cons` (3 `cons` nil))) (:) []
      = foldr (+) 0 (1 : (2 : (3 : [])))
      = 1 + (2 + (3 + 0))
      = 6
```
In the first case, no intermediate list is produced, whereas the second one has to allocate a list in memory, and then fold over that list. **Combinations `fold` and `build` can be optimized away by the compiler to avoid having to generate an intermediate list.** This optimization is known as *list fusion*, and is not limited to `foldr`: many other functions have rules with `build` in them, just search the source of `Data.List` for it!

As a final remark, we now know how to convert a lambda list to an ordinary list using `build`, but what about the reverse? Well, just pack the introductory paragraph of this section into a function, and you'll end up with

```haskell
unbuild :: [a] -> (a -> b -> b) -> b -> b
unbuild xs = \cons nil -> foldr cons nil xs
```





`build`'s strange type
----------------------

Let's ask GHCi for the type of `myLambdaList`:

```text
>>> :t myLambdaList
Num a => (a -> b -> b) -> b -> b
```

So a function building this list would map `myLambdaList` to `[a]`, therefore

```haskell
build' :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
--        | ^ Our lambda list, specified  |     ^ The generated list
--        | to b = [a], because of the    |
--        | return type of build'         |
build' g = g (:) []
```

This looks good on its own and works well for our lambda list, but it has one drawback: building is not the only thing it does. We would like to restrict `build` to only work on "clean" lambda lists that really do nothing but `cons/nil` elements together. However, we could also given an "unclean" buildable function to `build'`, for example

```haskell
unclean = \cons nil -> 1 `cons` reverse (2 `cons` (3 `cons` nil))
```

This has the right type `Num a => (a -> [a] -> [a]) -> [a] -> [a]`, but it doesn't just encode a lambda list, but also a computation *on* that lambda list (reversing). So `unclean` isn't *just* a lambda list, it's a little more, and that is already a little too much. Let's see what happens when we compare the short and long ways of folding over this list again, pretending the same rules apply:

```haskell
-- The short way: using the rule
foldr (+) 0 (build' unclean)
      = \cons nil -> 1 `cons` reverse (2 `cons` (3 `cons` nil)) (+) 0
      = 1 + reverse (2 + (3 + 0))
      -- Waaaaait. Reversing (2 + (3 + 0))? Type error!

-- The long way: building an intermediate list
foldr (+) 0 (build' myLambdaList)
      = foldr (+) 0 $ (\cons nil -> 1 `cons` reverse (2 `cons` (3 `cons` nil))) (:) []
      = foldr (+) 0 (1 : reverse (2 : (3 : [])))
      = foldr (+) 0 (1 : (3 : (2 : [])))
      = 1 + (2 + (3 + 0))
      = 6
```

The long way still does the right thing, but the short one is nonsense. We have to find a way to restrict `build` to not take lambda lists that do list operations somewhere inside, so that we leave the possibility open to "build" a list not using `build`, but any other function as well (such as `(+)` above). `forall` to the rescue!

```haskell
build :: forall a. (forall b. (a -> b -> b) -> b -> b) -> [a]
```

This looks like `build'`, but has a key difference: the `forall` in the first argument. This means that that argument has to work for all possible choices of `b` - in `build'`, you are allowed to build a function where `b = [a]`, and if you know that you can of course do list stuff with it, like `reverse`. However, the `forall` here asserts that the lambda list given has to work on *any* type `b`, not just on `[a]`, and you can't reverse an arbitrary `b`!

Well great, now we just lost us our connections to lists? Not really. `build g` only requires `g` to be a "clean" lambda list, but as soon as it gets one, it can decide to specify `b` to `[a]`. The key difference here is that it is `build` that decides "alright, I'll set `b = [a]`", while for our `build'` the call site does the specialization before handing over the actual function, "build also works for `b = [a]`, so I'll give it something with `[a]` in the first place".


When should I use `build`?
--------------------------

`build`'s single purpose is optimization for list functions. Unless you're writing a library dealing with generating and consuming lists, you most likely won't ever need to use `build`. However, when you're wondering again why `-O` compiled programs run so much faster than unoptimized ones, it is likely that `build` is part of the answer.
