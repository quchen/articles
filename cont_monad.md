How I understood the `Cont` monad
=================================

It took me a year longer than the other standard monads to understand what `Cont` does, but last week I made a big leap thanks to `#haskell` (more specifically because of parcs's explanations). Here's my story.



Continuations
-------------

You surely know the worker-wrapper transformation to introduce tail recursion. For example

```haskell
length []     = 0
length (_:xs) = 1 + length xs
```

is not tail recursive, but can be made so by packing the "1+" in another parameter, so that there's some cheap form of state (namely the length so far) in the function:

```haskell
length = length' 0
      where length' l []     = l
            length' l (_:xs) = length' (l+1) xs
```

This converts the return value of the function to a parameter containing what has been done already - its past, so to speak.

Now continuations can be seen as a similar transformation: you put something ordinary in a parameter instead of directly dealing with it. In case of a continuation, that parameter is what's do be done next.

```haskell
--                      |-----------| <- "Continuation" type. ("square maps an
--                      |           |     a to a continuation")
square :: Num a => a -> (a -> r) -> r
square x = \k -> k (x^2)
```

Instead of returning the value of `x^2`, it returns it wrapped in another function. You can see `k` as the future of `square x`. For example, if you set the future of `square x` to be the identity, you get

```
  square x id
= (\k -> k (x^2)) id
= id (x^2)
= x^2
```

A very useful way of picturing how this works in practice is this:

```haskell
square 2 $ \twoSquare -> (...)
```

This means that wherever you are in `(...)`, you have the result of `square 2` at hand by using `twoSquare`. So the intuitive understanding is that **continuations put the result value in a lambda parameter** (instead of returning it like ordinary functions). Let's see that in action:

```haskell
square x = \k -> k (x^2)
add x y  = \k -> k (x + y)
pythagoras x y = \k -> square x $ \squareX ->
                       square y $ \squareY ->
                       add squareX squareY $ \pythagoras ->
                       k pythagoras
```

Now calling `pythagoras 3 4` will result in the hypothetical result of `3^2 + 4^2`, so `pythagoras 3 4 id = 25`. We can modify this example to be even more continuation-style: square right now actually does two things, it squares and it takes in a number for squaring. These two steps can be separated: one function for getting a constant in the continuation chain, and one for squaring. The reason for this right now is "because why not", but it'll be useful for the explanations later.

```haskell
constant x = \k -> k x
square x   = \k -> k (x^2)
add x y    = \k -> k (x + y)
pythagoras x y = \k -> constant x -> \x' ->
                       constant y -> \y' ->
                       square x' $ \squareX ->
                       square y' $ \squareY ->
                       add squareX squareY $ \pythagoras ->
                       k pythagoras
```


The `Cont` monad
----------------

The purpose of the `Cont` monad is getting rid of all the explicit `\k -> k ...` you would have to introduce manually, much like `State` takes care of dragging along the `\s -> ...` explicitly.

`Cont` in practice consists of three functions, `return`, `>>=` ("bind") and `callCC`, which are easy, awful and very awful to understand by looking at the code, respectively. `Cont` itself is simply a wrapper around the type signature of a continuation like above (note that `square` took a parameter to produce a continuation, so it had an additional `a ->`).

```haskell
newtype Cont r a = Cont {runCont :: ((a -> r) -> r)}
```

### `return`

Let's start with `return`, which is fairly easy to read at least:

```
return a = Cont ($ a)
         = Cont (\k -> k a)
```

This can be seen as the equivalent of defining `a = ...`, but as a continuation, that gets wrapped in a `k`. **return encodes the future of a constant**. In the Pythagoras example above, `constant` is just the same as `return` here, minus the `Cont` wrapper.


### `>>=` (bind)

Conceptually, **bind composes two continuations**, so that the result of the first one is somehow fed to the second one. In the Pythagoras example, you can see the `\(...) ->` as the places where the monadic bind will now jump in. There are two levels to understanding this: one is the practical side, how to picture what `>>=` does, and one is how the code comes about. The practical side is pretty much what I wrote above, the technical side will be discussed in the following.

To do this plumbing, what has to be done? Well, we surely need a new continuation as a result, so bind should look like

```haskell
m >>= f = Cont $ \k -> (... k, f ...)
```

The parentheses should encode the future of the entire result. This result of course consists of two things: the result of the continuation `m`, and the one of whatever `f` creates. First, we have to calculate the result of `m` (as it is "earlier" in the bind chain), so we'll have something like

```haskell
m >>= f = Cont $ \k -> runCont m (... k, f ...)
```

`runCont m ...` is the actual result of the continuation up to `m` when we insert `(... k ...)` as its parameter function (like we did with `id` in the pythagoras example), and the whole thing is given the name `k`, just like `square x` is given the name `squareX` in the Pythagoras example. You'll also notice that the parameter function must have type `(a -> r)` so it can be plugged into a continuation, so let's make it a lambda:

```haskell
m >>= f = Cont $ \k -> runCont m (\l -> (... k, f, l ...))
```

Now we know that `f :: a -> Cont r b`, so the unknown part will surely involve some continuation. We also know that we don't have any other function to act on that besides `runCont` or its result, so this has to be the outermost call in there.

```haskell
m >>= f = Cont $ \k -> runCont m (\l -> runCont (... k, f, l ...))
```

Now everything's used up, all we have left is `k`, `f` and `l`. We know `f` has to be applied to something, `f k` doesn't comply with bind's type signature, and neither do the other combinations - the only possible way of combining these three variables is indeed `(f l) k`. The final result for bind is therefore

```haskell
m >>= f = Cont $ \k -> runCont m (\l -> runCont (f l) k)
```

Again, what this *does* is getting the value out of `m` by giving it a future, and that future consists of what `f` makes out of that value. The code for `>>=` may look a bit scary, but if you strip it of the wrappers/unwrappers, it becomes much shorter:

```haskell
(>>=) m f k = m (\l -> (f l) k)
```

`(\l -> (f l) k)` is what's given as the future of `m`'s content, and that future consists of modifying the contained value with `f`. Easy, huh? Just kidding. It's not. I still feel like understanding `>>=` is more about getting a good stomach feeling about what it does - which is simply composition of functions in odd notation. That explanation however is so simple that one is tempted of trying to find a deeper explanation, and it turns out that you have to bend over backwards of actually implementing this composition.



### `callCC`

The third function is `callCC`. It's not strictly part of the `Cont` monad, but is a very useful API function defined in the standard libraries, just like `State` has `put` and `get` so you don't have to worry about how to implement them every time.

`callCC` does a remarkable thing: beating `>>=` both in type signature as in implementation when it comes to unintuitive code. But let's start at the beginning: its purpose.

Suppose you build a continuation like

```haskell
abort :: a -> Cont r b
abort x = Cont $ \k -> x
```

This function doesn't use the continuation `k` at all. So what happens when you insert it somewhere along a long monadic computation? Well, as `x` has no future, all the following computations are simply not considered. It's like when you insert a `const 2` somewhere in a long chain of functions - everything on the right hand side will simply be discarded, because the result is `2` in any case.

```haskell
faculty 0 = 1
faculty n = n * factulty (n-1)
main = print $ (*2) . const 2 . faculty . (^1234) $ 100
```

This program will always print `4`, and it will omit creating the the faculty of `100^1234` (which is probably a good thing). A similar example using continuations is this:

```haskell
main = print . (`runCont` id) $ do a1 <- return 100;
                                   a2 <- return $ a1 ^ 1234
                                   a3 <- return $ faculty a2
                                   abort 2
                                   return (2 * a3)
```

When `abort` is reached, everything that follows is discarded - `2 * a3` is never executed.

Long story short: you can write functions that interrupt the control flow of a continuation chain. But sometimes you don't want a whole computation to abort if one of its parts aborts, sometimes it may be useful to have some sort of "scoping", a way of creating a section that ne separate from the outside continuation. This is what `callCC` is for: it is a **sandbox environment for continuations**. If you write `abort` in a `callCC` environment, it won't abort the entire continuation calculation, but only the one in `callCC` - whatever is wrapped around that remains untouched.

How could this be accomplished? Well, in order to not break the outer computation, `callCC` sets up its very own computation. The crux however is that it does not use this continuation, instead inserting the outer computation in it - it's like creating a new branch of the outer computation, encapsulating it in its own environment, and then merging that branch again.

So let's build this function like we've done it with `>>=` before. First, the desired type signature:

```haskell
callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
```

Oh noes. Well, at least we can see that the result will be of type `Cont r a`, so we know that

```haskell
callCC f = Cont $ \k -> runCont (...)
```

We also know `f`'s type, it is `(a -> Cont r b) -> Cont r a`, so we have to apply it to a `a -> Cont r b`. With the resulting `Cont r a`, all we can do is feeding it to `runCont`, which gives us

```haskell
callCC f = Cont $ \k -> runCont (f $ \a -> (...)) (...)
```

`l` has to map to a `Cont r b`,

```haskell
callCC f = Cont $ \k -> runCont (f $ \a -> Cont (\m -> (...))) (...)
```

Up to this point, basically everything got more complicated based on type-level arguments. Now here comes the crucial step: The inner continuation involving `\m ->` is the one that'll be run by `callCC` as it is supplied to `f`. As mentioned earlier however, we don't want to chain it directly in the outer continuation (i.e. the one around the whole `callCC` block). We therefore *discard* its continuation, and put in a copy of the outer continuation `k`:

```haskell
callCC f = Cont $ \k -> runCont (f $ \a -> Cont (\m -> k a)) (...)
```

Note how `m` is never used - `callCC` builds its own proper continuation environment, and then artificially inserts the outer continuation in that. The computation runs in an encapsulated environment. What's left as a final step is supplying the result of this encapsulated calculation to the outer continuation by applying the `runCont` to `k`, and we arrive at the definition of `callCC`,

```haskell
callCC f = Cont $ \k -> runCont (f $ \a -> Cont (\_ -> k a)) k
```

Summing it up, a version stripped of the newtype wrappers is then

```haskell
callCC f = \k -> (f $ \a -> (\_ -> k a)) k
```

`\k -> (...) k` supplies `k` to `(...)` to embed `callCC` in the outer continuation, and `(f $ \a -> (\_ -> k a))` runs `f` in a copy of the outer continuation.
