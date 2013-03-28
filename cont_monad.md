Haskell's continuation monad
============================

I am convinced that many complicated things in Haskell can - and have to be - understood on two levels: how to use it, and how it's implemented. Usage is usually much easier to grasp than implementation, which is why it should be the first approach. However, this practical knowledge doesn't tackle corner cases much, and may lead to unexpected surprises. This is where the inner workings of a function come into play, which is a lot easier to learn now that one knows what a function does; as a result, this feeds back to the intuitive side of things, eventually reaching "true" understanding. This article is split in these two levels: first the intuitive one, then the technicalities.










The concept of continuations
----------------------------

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
--                      |           |    'a' to a continuation")
square :: Num a => a -> (a -> r) -> r
square x = \k -> k (x^2)
```

Instead of returning the value of `x^2`, it returns it wrapped in another function. You can see `k` as the future of `square x`. For example, if you set the future of `square x` to be the identity, you get

```haskell
  square x id
= (\k -> k (x^2)) id
= id (x^2)
= x^2
```

A very useful way of picturing how this works in practice is this:

```haskell
square 2 $ \twoSquare -> (...)
```

This means that wherever you are in `(...)`, you have the result of `square 2` at hand by using `twoSquare`. So the intuitive understanding is that **continuations put the result value in a lambda argument** (instead of returning it like ordinary functions). Let's see that in action:

```haskell
square x = \k -> k (x^2)
add x y  = \k -> k (x + y)
pythagoras x y = \k -> square x $ \xSquare ->
                       square y $ \ySquare ->
                       add xSquare ySquare $ \result ->
                       k result
```

Now calling `pythagoras 3 4` will result in the hypothetical result of `3^2 + 4^2`, so `pythagoras 3 4 id = 25`.












What the `Cont` monad does
--------------------------

The purpose of the `Cont` monad is getting rid of all the explicit `\k -> k ...` you would have to introduce manually, much like `State` takes care of dragging along the `\s -> ...` explicitly. `Cont` itself is simply a wrapper around the type signature of a continuation like above in the Pythagoras example; note that `square` took a parameter to produce a continuation, so it had an additional `a ->`.

```haskell
newtype Cont r a = Cont {runCont :: ((a -> r) -> r)}
```


### `return`

`return` is what has been used in the Pythagoras example to wrap some expression in a `k` application,

```haskell
square :: (Num a) -> a -> (a -> r) -> r
square x = \k -> k (x^2)

squareCont :: (Num a) -> a -> Cont r a
squareCont x = return (x^2)
```

In other words, **`return` encodes the future of a constant**.



### `Cont`

`Cont` is our newtype wrapper that can be used to make a continuation out of arbitrary (well-typed) expressions. You can always **read the `k` in `Cont $ \k -> ` as "what to do afterwards"**; it is the starting point for further calculations.

What is also important about this is that in the code

```haskell
Cont $ \k -> (...)
```

the rest of the computation is run for each appearance of `k` in `(...)` independently. Consider the following examples:

```haskell
-- k isn't used at all. If this happens, the rest of the computation is
-- skipped entirely, and the final value is x.
exit x = Cont $ \k -> x

-- k is used once. This corresponds to the normal case, and serves as the
-- definition of the return function.
return x = Cont $ \k -> k x

-- k is used multiple times; the computation effectively splits up in two
-- independent calculations, one done with x, the other with y, and the results
-- are put in a tuple.
twice x y = Cont $ \k -> (k x, k y)
```


### `runCont`

`runCont` may just be an unwrapping function, but nevertheless there's an important interpretation of it: in

```haskell
runCont m $ \mVal -> (...)
```

**the lambda parameter `mVal` is the resulting value of the continuation of `m` when fully evaluated.** Contrary to ordinary functions, this value is not an independent object but exists only as a lambda parameter, but in the `(...)` block that doesn't make much of a difference.




### `>>=`

`>>=` composes two continuations. It takes a continuation and calculates its hypothetical result, then passes that result to a function generating a new continuation, and returns that continuation as its result, or short: **`>>=` passes the hypothetical result on**. This may be clearer when you compare it to the non-monadic way of writing things:

```haskell
-- Cont version (No 'do' notation to illustrate where >>= jumps in)
pythagoras :: (Num a) => a -> a -> Cont r a
pythagoras x y = return (x^2) >>= \xSquare ->
                 return (y^2) >>= \ySquare ->
                 return (xSquare + ySquare) >>= \result ->
                 return result
```

```haskell
-- Non-Cont version
pythagoras x y = \k -> ($ x^2) $ \xSquare ->
                       ($ y^2) $ \ySquare ->
                       ($ xSquare + ySquare) $ \result ->
                       k result
```

When you compare these, there's not much difference - basically the `\k` isn't there in the monadic version, and all `>>=` and `return` are `$` in the non-monadic one. In both cases, hypothetical results are calculated, and then passed on (via an explicitly named lambda parameter) to the rest of the computation.



### `callCC`

`callCC` on the conceptual side of things is actually quite easy to understand. Suppose you have the previously mentioned function

```haskell
exit x = Cont $ \_ -> x
```

When you evaluate this anywhere in a long `Cont` calculation, the end result will be `x`, no matter what the rest of the statements evaluate to. In imperative terms, this is like an early return statement. But breaking out all the way is not always what you want: sometimes you'd like to control where to break out from and to. If you're in a loop, a break statement might be much more useful than terminating the entire procedure. This concept in a more general version is what `callCC` is for.

Using `callCC` generally has the form of being applied to an explicit lambda, with a parameter called `exit`:

```haskell
foo :: Cont r a
foo = callCC $ \exit -> (...)
```

The `(...)` is a normal `Cont` computation, but now with a twist: you have the `exit` "function" at hand to break out early (admittedly it may sound weird to see `exit` parameter *itself* as a function at first). Suppose you want to convert some data to PDF format, but if there is an error it should abort and return an empty result.

```haskell
-- Create a PDF represented as a String from some Data
toPDF :: Data -> Cont r String
toPDF d = callCC $ \exit -> do
      when (broken d) $ exit ""
      makePDF d
```

The nice thing about `callCC` is that it is easily nestable, providing the functionality of short-circuiting arbirary levels. The following example first checks whether the data is broken (and terminates the procedure entirely). If it is fine, it examines whether it's not too long (resulting in a worded error message); if the data is alright but the format is dirty, it cleans it, and if everything works out alright it adds annoying eye candy.

```haskell
toPDF :: Data -> Cont r String
toPDF d = callCC $ \exit1 -> do
      when (broken d) $ exit1 "Data corrupt"
      d' <- callCC $ \exit2 -> do
            -- when = Control.Monad.when
            when (tooLong d) $ exit1 "Data too long" -- Jump out of everything
            when (dirty d) $ exit2 (clean d) -- Jump out of the inner callCC
            return $ decorateWithFlowers d
      return $ makePDF d'
```

To sum it up, **each `callCC` carries around its own `exit` function**, callable by the name of the lambda parameter. If you use this `exit` anywhere in its scope, the result of the corresponding `callCC` block will be `exit`'s argument; if you do not use it, the program works as if there was no `callCC $ \exit ->` in the first place.












The inner workings
------------------






### `return`

There's not much to say about `return`, the intuitive explanation pretty much directly translates to the code

```haskell
return x = Cont ($ x)
         = Cont (\k -> k x)
```

A constant `x` is wrapped in a continuation, which represents what is to be done next to that value.



### `>>=`

`>>=` always encapsulates the idea of taking a value out of a monadic expression, and feeding it to a monad-producing function. This is no different in the `Cont` case. First, the type signature is

```haskell
(>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b
```

We surely need a new continuation as a result due to that type, so it should look like

```haskell
m >>= f = Cont $ \after -> (... after, f ...)
```

`after` is short for *what to do afterwards*, as mentioned in intuitive section on the `Cont` wrapper. Now in order to get to that end result, we have to get the value out of the `m` first, and the only method at hand to do so is `runCont`,

```haskell
m >>= f = Cont $ \after -> runCont m (... after, f ...)
```

`runCont m` is a function waiting to receive its continuation function. Let's give it one, keeping in mind that providing a continuation with a lambda parameter allows us to access its value, as discussed in the previous section on `runCont`:

```haskell
-- mVal = "value contained in m"
m >>= f = Cont $ \after -> runCont m (\mVal -> (... after, f, mVal ...))
```

Now that extracted value `mVal` has to be applied to `f`, which is the function we want to feed with the result of the computation so far,

```haskell
m >>= f = Cont $ \after -> runCont m (\mVal -> (... after, (f mVal) ...))
```

To recap, up to this point we have done two things: given the continuation we'd like to produce the name `after`, and extracted the value `mVal` out of `m`. What's left is building a connection between the two, so that when the result is requested afterwards, the whole extraction stuff happens first:

```haskell
m >>= f = Cont $ \after -> runCont m (\mVal -> runCont (f mVal) after)
```

Again, what this *does* is getting the value out of `m` by, applying `f` to it, and wrapping the whole thing in a final continuation.



### `callCC`

Unfortunately `callCC` does a remarkable thing: beating `>>=` both in type signature as in implementation when it comes to not being intuitive. The inner workings of `callCC` can be described like this: it performs an ordinary new `Cont` calculation, but returns its final result with a crux. Instead of returning its own continuation, the continuation parameter of the *parent* block is inserted. You can view `callCC` as an independent calculation that in the end is merged into the parent continuation. This is also hinted by what `callCC` stands for, namely *call with current continuation*.

So let's build this function like we've done it with `>>=` before. First, the desired type signature:

```haskell
callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
```

At least we can see that the result will be of type `Cont r a`, so we know that

```haskell
callCC f = Cont $ \k -> runCont (...)
```

We also know `f`'s type, it is `(a -> Cont r b) -> Cont r a`, so we have to apply it to some function; with the resulting `Cont r a`, all we can do is feeding it to `runCont` (which is already there from above), which gives us

```haskell
callCC f = Cont $ \k -> runCont (f $ \a -> (...)) (...)
```

`l` has to map to a `Cont r b` due to `f`'s type signature,

```haskell
callCC f = Cont $ \k -> runCont (f $ \a -> Cont (\m -> (...))) (...)
```

Up to this point, basically everything got more complicated based on type-level arguments. Now here comes the crucial step: The inner continuation involving `\m ->` is the one that'll be run by `callCC` as it is supplied to `f`. As mentioned earlier however, we don't want to chain it directly in the outer continuation (i.e. the one around the whole `callCC` block). We therefore *discard* its continuation `m`, and put in a copy of the outer continuation `k`:

```haskell
callCC f = Cont $ \k -> runCont (f $ \a -> Cont (\m -> k a)) (...)
                                                    -- ^ k, not m!
```

This is the encapsulated execution of `callCC`; what's left as a final step is merging the result of this to the outer control flow by applying the `runCont` to `k`, and we arrive at the definition of `callCC`,

```haskell
callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
callCC f = Cont $ \k -> runCont (f $ \a -> Cont (\_ -> k a))) k
```

Summing it up, a version stripped of the newtype wrappers is then

```haskell
callCC f = \k -> (f $ \a -> (\_ -> k a)) k
```

`\k -> (...) k` supplies `k` to `(...)` to finally embed `callCC` in the outer continuation, and `(f $ \a -> (\_ -> k a))` runs `f` in a copy of the outer continuation, with its own private exit parameter.






`Cont` in the Haskell libraries
-------------------------------

One thing worth mentioning is that `Cont` in the Haskell library [`Control.Monad.Trans.Cont`][cont-docs] is defined in terms of a monad transformer (on `Identity`). The general purpose `Cont` wrapper I've used in this article is only a smart constructor named `cont` (lower case c), and the type signatures are all a little more general to account for possible uses as a transformer. Apart from these small items, it's pretty much what I've described above; most notably, the explanations still all hold even in the more general scenario.

[cont-docs]: http://hackage.haskell.org/packages/archive/transformers/latest/doc/html/Control-Monad-Trans-Cont.html




Thanks
------

I would probably still be in the dark about continuations if it wasn't for `#haskell`, most notably due to parcs's explanations. Thanks for that!
