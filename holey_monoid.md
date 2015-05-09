Holey monoids: monoids with left-open parameters
===

**Abstract.** A holey monoid is a monoid that one can think of has having two ways to add an element to. The first one is the usual monoid operation, which takes two values and concatenates them, for example `"lorem" <> "ipsum"`, to yield `"foobar"`. But sometimes, one might not want to insert an element, and defer the value to be inserted to later. In other words, what if one wanted to leave an element open, like in `foo = \later -> "lorem" <> later <> "dolor"`? Holey monoids provide a convenient abstraction to insert such parameters in a systematic fashion. Using holey monoids, the code to build the previous function would look something like

```haskell
foo :: String -> String
foo = run $ now "lorem" . later id . now "dolor"

-- GHCi:
>>> foo " ipsum "
"lorem ipsum dolor"
```



Usage examples
---

Building a database query string from chunks of text requires the programmer to be very careful: one forgotten call to a quotation function and you've got a gaping security hole in your application. Wouldn't it be nice if you could build the query string, and have it automatically quote all user-facing fields? A DSL that

```haskell
type Query r = HoleyMonoid Text r r
finalize = run

query :: Query
query = $ select "user"
        . userInput number
```
select :: Show a => a -> HoleyMonoid String r r
select x = now "SELECT " ++ escape x ++ "\n"

from :: Show a => a -> HoleyMonoid String r r
from x = now "FROM " ++ escape x ++ "\n"





Implementing holey monoids
---

Holey monoids are based on a type with three parameters,

```haskell
data HoleyMonoid m r a = ...
```

The three parameters each have a specific purpose:

- `m` is the underlying monoid that we'd like to be able to insert holes into. In the example given in the abstract, `m = String`.

- `r` is the type of the result when the holey monoid has all its holes filled with values. In the abstract, this would be `String`. However, due to reasons becoming apparent later, this will be left polymorphic as long as it can.

- `a` is a function of zero or more arguments that eventually yields an `r`. Each time a hole is encountered in the holey monoid, this automatically adds another parameter to the function represented by this `a`. In the example form the abstract, `a = String -> String`.

### API functions

We'd like to construct a couple of operations to use holey monoids. In particular, there's need for the following ones:

- Since a holey monoid is a beefed up monoid, it has to provide an empty holey monoid (lifted `mempty`), and a concatenation function to glue two together (lifted `mappend`). Call them `memptyH` and `mappendH`.

- Adding values to a holey monoid requires one operation for inserting a value directly (`now`), and one for a hole to be filled later (`later`).

- To extract the built up function from a holey monoid, it needs to be stripped to an ordinary function. Following the usual Haskell names, this one should be called `runHoley`.

### Discussion of the API functions' types

Let's combine the previous thoughts to figure out what the types of the API functions should be. Remember that `HoleyMonoid m r a` is our type in question here. What do those type parameters amount to for the API functions?

- `now x` represents the holey monoid corresponding to the monoidal value `x :: m`, so it should have type `HoleyMonoid m ? ?`.

- `memptyH` describes an empty "holey" value, that when run (via `runHoley`) should result in simply the normal `mempty`. The underlying monoid should be, well, a monoid, so the `m` type parameter here should be `Monoid m => m`. This means that `memptyH :: Monoid m => HoleyMonoid m ? ?`. The `a` type parameter


now :: m -> HoleyMonoid m r r
later :: (a -> m) -> HoleyMonoid m r (a -> r)
memptyH :: HoleyMonoid m r r
mappendH :: HoleyMonoid m b c -> HoleyMonoid m a b -> HoleyMonoid m a c
runHoley :: HoleyMonoid m m a -> a