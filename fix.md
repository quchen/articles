The `fix` function
==================

The fundamental property of a recursive function is that it can call (refer to)
itself. For example,

```haskell
factorial n | n <= 0    = 1
            | otherwise = n * factorial (n-1)
```

But what if the function does not have a name (that is known within its body), i
.e. the name `factorial` is not known on the right hand side? Well, this is
what `fix` is for. It's a function with peculiar type and implementation,

```haskell
fix :: (a -> a) -> a
fix f = f (fix f)
   -- = f (f (f (f ...
```

that allows turning non-recursive functions into recursive ones. You may have
heard of a function that satisfies this in untyped lambda calculus, it's known
as the Y combinator. Unfortunately, neither type nor implementation are very
good at telling you what this function does if you're not already aware of it.

The following will motivate the `fix` function using three different approaches.
 I hope that at least one of them makes it "click" for every reader.



Approach A: Intuition
---------------------

Intuitively, I think this can be best be understood by analogy with a more
explicit version. Let's rewrite the factorial example from above a little
different:

```haskell
factorial = go where
      go n | n <= 0    = 1
           | otherwise = n * go (n-1)
```

This makes `factorial` itself non-recursive by putting the recursion one level
deeper down, in a helper function named `go`. And now it's ready to be
transformed into `fix` form, which is simply

```haskell
factorial = fix go where
      go rec n | n <= 0    = 1
               | otherwise = n * rec (n-1)
```

Oh wait, what just happened? `go` no longer is recursive, it instead has a
parameter named `rec`. **`fix` takes a function, and whenever the (first)
parameter of that function is called, it refers to itself entirely.** In other
words, `fix` allows creating recursive from non-recursive definitions.



Approach B: Separating recursion
--------------------------------

Consider the factorial function again, but written using `let` and `if` this
time,

```haskell
factorial = let go n = if n <= 0 then 1 else n * go (n-1)
            in  go
```

The code can be written this way because all definitions made inside `let` are
visible everywhere in that `let`; in other words, Haskell's `let` allows
self-contained recursion. (Other languages call this behaviour more explicitly
`letrec` instead of `let`.)

Suppose the goal is separating the recursion from the rest of the program. We
will do this in multiple small steps.

```haskell
factorial = let go n = if n <= 0 then 1 else n * go (n-1)
            in  go

          -- Abstract over go and n by creating a dummy lambda with
          -- parameters rec and m. Note how applying this to go and
          -- n yields the previous definition again.
          = let go n = (\rec m -> if m <= 0 then 1 else m * rec (m-1)) go n
            in  go

          -- Since the parenthesis is now independent of go, move it
          -- to its own line.
          = let go n = f go n
                f = \rec m -> if m <= 0 then 1 else m * rec (m-1)
            in  go

          -- Drop the trailing n in go, rewrite the lambda parameters in f
          -- a bit. Now the recursion (go) is spearate from the other
          -- logic (f).
          = let go = f go
                f rec m = if m <= 0 then 1 else m * rec (m-1)
            in  go

          -- Finally, split up the let in two let blocks to emphasize the
          -- independence of the contents.
          = let f rec m = if m <= 0 then 1 else m * rec (m-1)
            in let go = f go
               in  go
```

And now all that's left is identifying `let go = f go in go` as a valid
implementation for `fix f`, which you can verify via

```haskell
fix f = let go = f go in go
      = let go = f go in f go
      = f (let go = f go in go)
      = f (fix f)
```

So the above refactoring becomes

```haskell
          -- (continued)
factorial = let f rec m = if m <= 0 then 1 else m * rec (m-1)
            in  fix f
```

**Separating recursion from other logic naturally yields `fix`.**



Approach C: Tying the knot
--------------------------

Let's start in the reverse direction of the last approach. Begin with an almost
right function,

```haskell
brokenFactorial = \rec -> (\n -> if n <= 0 then 1 else n * rec (n-1))
```

which is not recursive because there is no way to reference itself. Instead,
the "recurse" parameter `rec` was provided as a lambda binding. If we can
somehow insert the entire expression into this parameter, we'd end up with a
recursive function. In other words, we're looking for a function that does this:

```haskell
unknown rec n = rec (unknown rec) n
```

And as you can probably already tell, this makes `unknown = fix`, yielding

```haskell
factorial = fix brokenFactorial
          = fix (\rec -> (\n -> if n <= 0 then 1 else n * rec (n-1)))
          = fix (\rec n -> if n <= 0 then 1 else n * rec (n-1))
```

**`fix` allows a function to refer to itself by calling its first parameter.**



Examples
--------


### Basic functions

Here are a couple of examples, first written using (the probably more readable
and more common way of) explicitly named functions, and then using `fix`.

```haskell
-- n-th Fibonacci number, naive implementation

fibo = go where
      go n | n <= 1    = n
           | otherwise = fibo (n-1) + fibo (n-2)

fibo = fix go where
      go rec n | n <= 1    = n
               | otherwise = rec (n-1) + rec (n-2)
```

```haskell
-- Prelude.map

map f = go where
      go (x:xs) = f x : go xs
      go _ = []

map f = fix go where
      go rec (x:xs) = f x : rec xs
      go _ _ = []
      -- go was given its own name here for readability, but note
      -- that it is not a recursive definition.
```

```haskell
-- Prelude.foldr

foldr f z = go where
      go (x:xs) = x `f` go xs
      go _ = z

foldr f z = fix go where
      go rec (x:xs) = x `f` rec xs
      go _ _ = z
```

```haskell
-- Prelude.zipWith

zipWith f = go where
      go (x:xs) (y:ys) = f x y : go xs ys
      go _ _ = []

zipWith f = fix go where
      go rec (x:xs) (y:ys) = f x y : rec xs ys
      go _ _ _ = []
```

You can see where this is going: instead of making a recursive call to itself,
a function with a "recursive" first argument is created that stands as a
placeholder for "recurse here". `fix` then takes this recursive placeholder and
makes it actually refer to itself.


### Ad-hoc monadic loops

`fix` is sometimes used to create loops in monadic code:

```haskell
fix $ \loop -> do
      x <- getLine
      case readMaybe x :: Maybe Int of
            Nothing -> putStrLn "Parse error, expected Int" >> loop
            Just n  -> return n
```

This reads an `Int` from STDIN, but recurses back to itself if the input wasn't
right. This can easily be expanded to carry around some state, for example this
counts the number of bad inputs so far:

```haskell
(\f -> fix f 1) $ \loop n -> do
      x <- getLine
      case readMaybe x :: Maybe Int of
            Nothing -> do putStrLn "Input " ++ show n ++ " wrong, expected Int"
                          loop (n+1)
            Just n  -> return n
```

Monadic loops are arguably more readable than for non-monadic ones, since
non-monadic code requires to keep track of a form of recursion state using a
second parameter for `fix`' argument. In monadic code on the other hand, the
states and effects can be implicit, so there's only code of the form

```haskell
fix $ \loop -> ... loop ...
```

which quite plainly stands for "when you reach `loop`, restart after the `->`
again".