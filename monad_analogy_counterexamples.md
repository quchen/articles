# Monad analogy counterexamples

## Monads are containers

No.

### Counterexample 1: Proxy

A `Proxy` is a data type that does not contain anything and does nothing except
being `Proxy`. It’s the pathological example of a monad.

```haskell
data Proxy a = Proxy

instance Functor Proxy where
    fmap _ _ = Proxy
instance Applicative proxy where
    pure _ = Proxy
    _ <*> _ = Proxy
instance Monad Proxy where
    _ >>= _ = Proxy
```

### Counterexample 2: Parser

A simple parser type is

```haskell
newtype Parser a = Parser (String -> [(String, a)])
```

This stands for taking an input `String`, and generating a list of remaining
input and parsed data structure pairs.

This type can be given a `Monad` instance, and we can write the usual things we
like to do with parsers with it. At no point however does a value of type
`Parser a` contain an `a` though. We can work with hypothetical `a`s in monadic
binds, but those don’t exist until we step out of the monadic computation,
unwrap, and apply the parser to an input stream.



## Monads are pipelines

Chains of `>>=` are not evaluated from left to right in any way different than
chains of function applications (because `>>=` *is* an ordinary function). And
functions can be evaluate in any order the compiler wants them to, up to data
dependencies which demand a result to be there before being able to continue.

### Counterexample: Reverse State

The reverse state monad is an absurd example of how to break left-to-right
resembling data flow.

The ordinary `State` Monad is defined as follows:

```haskell
newtype State s a = State { runState :: a -> (s, a) }

instance Functor State ...
instance Applicative State where pure x = State (\s -> (s, x)) ...
instance Monad State where
    m >>= f = State (\s1 ->
        let (s2, x2) = runState m s1
            (s3, x3) = runState (f x2) s2
        in (s3, x3)
```

The idea here is to compute the current value depending on the current state,
and then doing a state transition using the function `f` based on the new state.

In writing this, one can make a small but consequential mistake, by writing

```haskell
m >>= f = State (\s1 ->
    let (s3, x2) = runState m s2
        (s2, x3) = runState (f x2) s1
    in (s3, x3)
```

where `s2` and `s3` have been mixed up in the `let` block. This yields the
behaviour that the states go in the other direction as the values. Putting this
into the pipeline analogy means that there is flow in both directions at the
same time while working towards a common goal. There is a short [article by Luke
Palmer][revstate] that shows how to calculate the Fibonacci series with this,
where it seems like the Fibonacci series is constructed by supposing it’s there
and then taking all elements away from it!

[revstate]: https://lukepalmer.wordpress.com/2008/08/10/mindfuck-the-reverse-state-monad/
