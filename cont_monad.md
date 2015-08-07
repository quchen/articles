Haskell's continuation monad
============================

I am convinced that many complicated things in Haskell can - and have to be -
understood on two levels: how to use it, and how it's implemented. Usage is
usually much easier to grasp than implementation, which is why it should be the
first approach. However, this practical knowledge doesn't tackle corner cases
much, and may lead to unexpected surprises. This is where the inner workings of
a function come into play, which is a lot easier to learn now that one knows
what a function does; as a result, this feeds back to the intuitive side of
things, eventually reaching "true" understanding. This article is split in
these two levels: first the intuitive one, then the technicalities.










The concept of continuations
----------------------------

You surely know the transformation to introduce better recursion by packing the
result into an accumulator. For example

```haskell
length []     = 0
length (_:xs) = 1 + length xs
```

will build a large stack of `1+1+1+...` operations, but this issue can be fixed
by packing the "1+" in another parameter, so that there's some cheap form of
state (namely the length so far) in the function:

```haskell
length = length' 0 where
      length' l []     = l
      length' l (_:xs) = length' (l+1) xs
```

This converts the return value of the function to a parameter containing what
has been done already - its past, so to speak.

Now continuations can be seen as a similar transformation: you put something
ordinary in a parameter instead of directly dealing with it. In case of a
continuation, that parameter is what's to be done next.

```haskell
--                      |-----------| <- "Continuation" type. ("square maps an
--                      |           |    'a' to a continuation")
square :: Num a => a -> (a -> r) -> r
square x = \k -> k (x^2)
```

Instead of returning the value of `x^2`, it returns it wrapped in another
function. You can see `k` as the future of `square x`. For example, if you set
the future of `square x` to be the identity, you get

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

This means that wherever you are in `(...)`, you have the result of `square 2`
at hand by using `twoSquare`. So the intuitive understanding is that
**continuations put the result value in a lambda argument** (instead of
returning it like ordinary functions). Let's see that in action:

```haskell
square :: Num a => a -> (a -> r) -> r
square x = \k -> k (x^2)

add :: Num a => a -> a -> (a -> r) -> r
add x y  = \k -> k (x + y)

pythagoras :: Num a => a -> a -> (a -> r) -> r
pythagoras x y = \k ->
      square x $ \xSquare ->
      square y $ \ySquare ->
      add xSquare ySquare $ \result ->
      k result
```

In the end the result of the addition is bound to `result`, which is what we
want to have. Since we're staying in continuation-passing style, `pythagoras`'
continuation `k` is applied to the result.

Now calling `pythagoras 3 4` will result in the hypothetical result of
`3^2 + 4^2`, so `pythagoras 3 4 id = 25`.

The important take-away message here, again, is that the following two lines
are equivalent: both of them calculate `f x`, and then make the result
available inside the `(...)` under the name `y`.

```haskell
y = f x
(...)

f x $ \y -> (...)
```

The same piece of pythagoras code written using the `Cont` type would look like
this:

```haskell
square :: Num a => a -> Cont r a
square x = Cont $ \k -> k (x^2)

add :: Num a => a -> a -> Cont r a
add x y = Cont $ \k -> k (x + y)

pythagoras, pythagoras' :: Num a => a -> a -> Cont r a
pythagoras x y = do
      xSquare <- square x
      ySquare <- square y
      result <- add xSquare ySquare
      pure result
-- or in equivalent explicit bind notation:
pythagoras' x y =
      square x >>= \xSquare ->
      square y >>= \ySquare ->
      add xSquare ySquare >>= \result ->
      pure result
```

But now this can be refactored because of all the typeclasses `Cont` ships
with. All of the functions now take `Cont` as parameters, and the intermediate
continuations are passed through automatically.

```haskell
-- Square a Cont
square :: Num a => Cont r a -> Cont r a
square = fmap (^2)

-- Add two Conts
add :: Num a => Cont r a -> Cont r a -> Cont r a
add = liftA2 (+)

-- Pythagoras two conts
pythagoras :: Num a => Cont r a -> Cont r a -> Cont r a
pythagoras x y = add (square x)
                     (square y)

-- GHCi
>>> (`runCont` id) (pythagoras (pure 3) (pure 4))
25
```










What `Cont` does
----------------

The purpose of `Cont` is getting rid of all the explicit `\k -> k ...` you
would have to introduce manually, much like `State` takes care of dragging
along the `\s -> ...` explicitly. `Cont` itself is simply a wrapper around the
type signature of a continuation like above in the Pythagoras example; note
that `square` took a parameter to produce a continuation, so it had an
additional `a ->`.

```haskell
newtype Cont r a = Cont { runCont :: (a -> r) -> r }
```

We will now be building our way up to the `Monad` instance, defining `Functor`
and `Applicative` on the way.



### Functor

The Functor instance is probably the most important one you have to learn for
`Cont`: once you understand it, `Applicative` and `Monad` will come naturally.
I was very surprised myself once I realized this.

We'll be constructing the `Functor` based on the unwrap-apply-wrap idea. The
box picture of certain Haskell types might sometimes be awkward, but for
certain computations it's a helpful model nevertheless. So for a `Functor`
instance, what we want to do to get `fmap :: (a -> b) -> Cont r a -> Cont r b`
is:

1. Extract the value of type `a` out of the `Cont` we're given.
2. Apply the function `f` to that value, yielding a new value of type `b`.
3. Wrap this value in a `Cont` again.

The hardest part is the first one, I believe. But let's start simple: by
writing as much as we know down. We know that we want a `Cont` again in the
end, so the code will look like this:

```haskell
fmap f cx = Cont ...
```

Because of `Cont`s type `((a -> r) -> r) -> Cont r a`, we can see that the
argument given to `Cont` is of type `(a -> r) -> r`. The `a -> r` part of that
can be bound to a value named `k`:

```haskell
fmap f cx = Cont $ \k -> ...
```

Now let's tackle the first problem: extracting a value `x :: a` out of the
given `cx :: Cont r a`. Remember the pythagoras example from above, where
instead of returning values they were provided to us as a lambda parameter?
That's exactly what happens here again.

```haskell
fmap f cx = Cont $ \k ->
                   runCont cx $ \x ->
                   ...
```

This gives us access to `x :: a`, so 1. is finished! The second issue is
applying `f` to that value, which is trivial:

```haskell
fmap f cx = Cont $ \k ->
                   runCont cx $ \x ->
                   let result = f x
                   in  ...
```

now we have our result as a value, next is step 3, making a proper `Cont`
again. We could simply return `result` from the function.  But that would
completely ignore the `k`, and thus break the continuation (ignoring all future
continuations). But what have we learned before? "Don't return values directly,
return them wrapped in the continuation parameter!"

```haskell
fmap f cx = Cont $ \k ->
                   runCont cx $ \x ->
                   let result = f x
                   in  k result
```

And that's it! Inlining `result` gives us a readable Functor instance:

```haskell
instance Functor (Cont r) where
      fmap f cx = Cont $ \k ->
                         runCont cx $ \x ->
                         k (f x)
```

To reiterate, here are the three steps we've done again:

1. Extract the value out of a computation `cx`. This is done by
   `runCont cx $ \x ->`, which gives us access to said vaue in the
   lambda's body.
2. Transform that value using the mapping function `f`.
3. Wrap the new value in the continuation parameter `k` again.



### Applicative

I promised this would be easy when you've understood the `Functor` instance,
let's hope I can stand true to my promise.

`pure` does not have to do any extraction or transformation, so steps 1 and 2
fall away. All that's left to do is to take our general form

```haskell
pure :: a -> Cont r a
pure x = Cont $ \k -> ...
```

The resulting value is provided by `x` already, and instead of returning it
directly in the lambda's body, we wrap it in `k`.

```haskell
pure x = Cont $ \k -> k x
```

Done.

Up next: `<*>`. This time, we have two `Cont` values: one holding a function,
and one holding the value to apply it to. We'll extract the function, then
extract the value, apply the function to the value, and wrap it in `k` again.

```haskell
cf <*> cx = Cont $ \k ->
                   runCont cf $ \f ->   -- extract f
                   runCont cx $ \x ->   -- extract x
                   k (f x)              -- apply f to x, wrap in k
```

And we're done!

(Bonus joke for the experienced Haskeller: imagine what happens when we switch
the second and third lines above. Nothing? Now imagine a `ContT` transformer
doing this.)



### Monad

Since we already have the `Applicative` we can set `return = pure`. Now for
`cx >>= f`, you guessed it: extract the value `x` out of the provided
`Cont` `cx`, apply the function `f` to it (yielding a new `Cont`), extract the
value out of that `Cont`, and wrap it up in `k` again.

```haskell
cx >>= f = Cont $ \k ->
                  runCont cx $ \x ->
                  runCont (f x) $ \fx ->
                  k fx
```

As a bonus, we could also define `join`, which is similarly simple.

```haskell
join' :: Cont r (Cont r a) -> Cont r a
join' ccx = Cont $ \k ->
                   runCont ccx $ \cx ->   -- extract inner continuation
                   runCont cx $ \x ->     -- extract value of inner continuation
                   k x                    -- wrap value in k again
```





### Special API function: `callCC`



#### Purpose

Suppose you want a `Cont` value to break out of the evaluation and return a
value `x`. You would implement this by simply ignoring the continuation
parameter `k`:

```haskell
exit x = Cont $ \_k -> x
```

If you place this anywhere in a large `Cont` calculation, the final result will
simply be `x` if `exit` is evaluated at any point.

`callCC` creates a sandboxed environment for this idea: instead of leaving the
entire calculation, only the sandbox is jumped out of. It is usually used like
this:

```haskell
foo :: Cont r a
foo = callCC $ \exit -> ...
```

inside the `...`, you now have an `exit` function that you can use. When this
function is evaluated, its argument will become the value of the `callCC` block.

An example: suppose you want to convert some data to PDF format, but if there
is an error it should abort and return an empty result.

```haskell
-- Create a PDF represented as a String from some Data
toPDF :: Data -> Cont r String
toPDF d = callCC $ \exit -> do
      when (broken d) (exit "ERROR")
      makePDF d
```

The nice thing about `callCC` is that it is nestable, providing the
functionality of short-circuiting arbirary levels. The following example first
checks whether the data is broken (and terminates the procedure entirely). If
it is fine, it examines whether it's not too long (resulting in a worded error
message); if the data is alright but the format is dirty, it cleans it, and if
everything works out alright it adds annoying eye candy.

```haskell
toPDF :: Data -> Cont r String
toPDF d = callCC $ \exit1 -> do
      when (broken d) (exit1 "Data corrupt")
      d' <- callCC $ \exit2 -> do
            when (tooLong d) (exit1 "Data too long") -- Jump out of everything
            when (dirty d) (exit2 (clean d)) -- Jump out of the inner callCC
            return (decorateWithFlowers d)
      return (makePDF d')
```

To sum it up, **each `callCC` carries around its own `exit` function**,
callable by the name of the lambda parameter. If you use this `exit` anywhere
in its scope, the result of the corresponding `callCC` block will be `exit`'s
argument; if you do not use it, the program works as if there was no
`callCC $ \exit ->` in the first place.

Some readers coming from an imperative background might recognize this
jumping-around-behaviour as similar to statements like `break`, `continue` and
`goto`. `callCC` can indeed be seen as a generalization of these concepts.
`callCC` should not be used too lightheartedly, or the control flow of the
program might become an obfuscated mess.



#### Implementation

Recall we had our "hard exit function"

```haskell
exit x = Cont $ \_ -> x
```

and the idea that `callCC` gives us a sandbox where a function like this is
locally contained.

The idea behind the implementation is building an almost fully contained `Cont`
calculation (which is evaluated inside the `callCC`). Once it terminates, its
value is extracted, and fed to the parent `Cont` calculation (the one
containing `callCC`). This gives us the structure

```haskell
callCC' = Cont $ \k ->
                 runCont "<sandboxed calculation>" $ \result ->
                 k result
```

where as before we have the usual `Cont $ \k ->` wrapper in which we build a
value, and then return `k <value>` in the end. What's left to ask now is how
the sandboxed calculation should be built up.

The most primitive calculation that has to go in there somehow is simpy `exit`.

```haskell
callCC' :: Cont String a
callCC' = Cont $ \k ->
                 runCont (exit "foobar") $ \result ->
                 k result
```

When evaluating this, the inner continuation encounters the `exit`, the final
result of everything `Cont` is set to `"foobar"` by ignoring `k`, and thus
everything after the second `($)` is never evaluated. How dull. But we can
fix this! If we abort the inner `Cont` calculation not with `"foobar"` but with
`k "foobar"`, then calling `exit` short-circuits out once more, but since it's
using `k` now, it will continue as if its value was `"foobar"`!

```haskell
callCC' :: Cont r String
callCC' = Cont $ \k ->
                 runCont (exit (k "foobar")) $ \result ->
                 k result
```

You can see that this is actually equivalent to `return "foobar"` when you
refactor the code a bit:

```haskell
callCC' = Cont $ \k -> runCont (exit (k "foobar")) $ \result -> k result
        = Cont $ \k -> runCont (Cont $ \_ -> k "foobar") $ \result -> k result
        = Cont $ \k ->         (       \_ -> k "foobar") $            k
        = Cont $ \k -> (\_ -> k "foobar") k
        = Cont $ \k -> k "foobar"
        = return "foobar"
```

Great, so now we've achieved a detour version of `return` using `exit`: instead
of directly returning a value, we construct a new computation that
short-circuits out with that value.

And now the key idea: what if we only conditionally evaluate `exit` here?

```haskell
callCC' f = Cont $ \k ->
                   runCont (f (exit (k "foobar"))) $ \result ->
                   k result
```

Now it depends on `f` what happens:

1. `f` uses its argument. The `runCont` will evaluate the result of whatever
   `f` produces, and eventually encounter the `exit` function. The entire thing
   short-circuits with `k "foobar"`, so `"foobar"` is the value passed on to
   `result`.
2. `f` ignores its argument, i.e. `f` is a constant function that maps to
   `Cont r a`. Whatever that value is, it is passed to `result`. No
   short-circuiting here.

This is already very close to `callCC`, only one last step is missing:
abstracting the `"foobar"` away, since that's hardly the only value we ever
want to short-circuit with.

```haskell
callCC f = Cont $ \k ->
                  runCont (f (\x -> exit (k x))) $ \result ->
                  k result

      where exit x = Cont $ \_ -> x
```

And there you have it: `callCC`! Let's reiterate the design: take a `Cont`
computation that might short-circuit out by discarding its own continuation
(the `_` in `exit`), but wrap the value to exit with with the parent
continuation `k`. On short-circuiting, the parent continuation is used for
further calculations. On not short-circuiting, the `exit` function is never
called, and the inner `Cont` is evaluated as if there was no `callCC` around in
the first place, and it was simply evaluated along with its parent calculation.



`Cont` in the Haskell libraries
-------------------------------

One thing worth mentioning is that `Cont` in the Haskell library
[`Control.Monad.Trans.Cont`][cont-docs] is defined in terms of a monad
transformer (on `Identity`). The general purpose `Cont` wrapper I've used in
this article is only a smart constructor named `cont` (lower case c), and the
type signatures are all a little more general to account for possible uses as a
transformer. Apart from these small items, the other notable difference is the
pointfree style. For example, `fmap` would be defined as

```haskell
fmap f cx = Cont $ \k -> runCont cx (k . f)
```

I find this to be completely incomprehensible, so I chose to present the more
pointful version here. Similar thoughts apply to all the other sections.

[cont-docs]: http://hackage.haskell.org/packages/archive/transformers/latest/doc/html/Control-Monad-Trans-Cont.html



Acknowledgements
----------------

I would probably still be in the dark about continuations if it wasn't for
`#haskell`, most notably due to parcs' and [Chris Done][chrisdone]'s
explanations. Thanks for that!

[chrisdone]: http://chrisdone.com/
