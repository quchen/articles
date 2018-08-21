# Tricky Haskell questions

These questions have insightful answers about some deep detail of Haskell.

# let vs λ

Why do we have »let« syntax when we also have lambdas? What’s the difference
between these two expressions?

```haskell
(\x -> <expr>) y
let x = y in <expr>
```

# newtype vs strict data

What is the difference between these two types?

```haskell
data Data a = Data !a
newtype Newtype a = Newtype a
```

(Note: Bangs in data definitions are standard Haskell.)

# Overflowing with foldl'

`foldl' (+) (0 :: Int) [1..100000]` – in what way can a sufficiently stupid
compiler overflow here?

# Pest or Cholera?

Does `foldr (+) (0 :: Int)` overflow the heap or the stack? What about
`foldl (+) (0 :: Int)`?
