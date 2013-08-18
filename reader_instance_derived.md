The Reader Monad instance
===========================



`return`
--------

`return` is a function that yields an action that, applied to whatever environment, yields the same value:

```haskell
return x = \environment -> x -- Ignore environment, result is always x
```

`environment` is never used, so let's abbreviate it,

```haskell
return x = \_ -> x
```

Remember that `\_ -> x` is just `const x`, giving us

```haskell
return x = const x
```

And now we've got a trailing `x` on both sides so let's get rid of that as well, and we have

```haskell
return = const
```




Bind/`>>=`
----------

We'll construct `m >>= f` in three parts: first, we're getting a value "out" of `m` (by applying it to the environment), then we're applying `f` to it, and finally it's all packed up again so the type system is happy (and nothing remains dangling or unused). That idea in code then looks like the following (you can read it top-to-bottom, imperative-like):

```haskell
m >>= f = \environment ->        -- `m >>= f` is a function that takes one
                                 -- parameter, which we call the environment.
                                 -- All `(->) r` functions will read from this
                                 -- environment (of type `r`).

      let mValue = m environment -- Get the "value" of `m` out by applying it to
                                 -- the environment. `m` has type `(->) r a`,
                                 -- so `mValue` has type `a`.

          fResult = f mValue     -- Apply f to the previously obtained value in
                                 -- order to get a new function out again.
                                 -- Note that `fResult :: (->) r b`, with
                                 -- `f :: a -> (->) r b`.

      in  fResult environment    -- Clean up. We can't just use `fResult` here,
                                 -- since there's still the `\environment` open
                                 -- from above. Since whatever `f` produces
                                 -- should in turn read from the same
                                 -- environment, we pass that to `fResult` too.
```

That's it, this is a valid implementation of `>>=`. We can refactor the code a little though, and you'll see how it's the same as the standard Reader instance. First, let's get rid of all the comments and rename `environment` to `r`:

```haskell
m >>= f = \r ->
      let mValue = m r
          fResult = f mValue
      in  fResult r
```

Now it's time for some inlining. `fResult` is calculated and immediately applied to `r`, so we can combine the last two lines into one (replace all `fResult` with `f mValue`), giving us

```haskell
m >>= f = \r ->
      let mValue = m r
      in  f mValue r
```

The same step again with `mValue` in the last two lines (insert `m r` wherever you see `mValue`) yields

```haskell
m >>= f = \r ->
      f (m r) r
```

And once you put that into one line, you'll end up with

```haskell
m >>= f = \r -> f (m r) r
```

And that is the cryptic definition of `>>=` you see everywhere. Again, the thinking has been done in the very first code snippet, the rest is just refactoring.