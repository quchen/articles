Functions in comprehensible notation
====================================

Sequence/mapM
-------------

```haskell
sequence :: Monad m => [m a] -> m [a]

sequence = mapM id

sequence [a, b, ..., c] = do
      x <- a
      y <- b
      ...
      z <- c
      return [x, y, ..., z]
```



```haskell
mapM :: Monad m => (a -> m b) -> [a] -> m [b]

mapM f = sequence . map f

mapM f [a, b, ..., c] = do
      x <- f a
      y <- f b
      ...
      z <- f c
      return [x, y, ..., z]
```



Foldl/foldr
-----------

`foldr f z xs` replaces every `(:)` in `xs` with `f`, and the `[]` with `z`. This also explains how `foldr (:) []` is the identity on lists: it replaces `(:)` with `(:)` and `[]` with `[]`.

```haskell
-- (\x acc -> newAcc) -> initialValue -> list -> result
foldr :: (a -> b -> b) -> b -> [a] -> b

foldr f z (x1 : x2 : x3 : ... : []) = x1 `f` (x2 `f` ... (xn `f` z)...))
```
Graphical version:
```
  :                            f
 / \                          / \
1   :         foldr f z      1   f
   / \           ==>            / \
  2   :                        2   f
     / \                          / \
    3   :                        3   f
       / \                          / \
      4  []                        4   z
```


```haskell
-- (\acc x -> newAcc) -> initialValue -> list -> result
foldl :: (a -> b -> a) -> a -> [b] -> a

foldl f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f`...) `f` xn
                            == f ( ... (f (f (f (f z x1) x2) x3) x4) ...) xn
```
Graphical version:
```
  :                                  f
 / \                                / \
1   :         foldl f z            f   4
   / \           ==>              / \
  2   :                          f   3
     / \                        / \
    3   :                      f   2
       / \                    / \
      4  []                  z   1
```



foldM, filterM
--------------

`foldM` is like foldl, but the accumulator is monadic.
```haskell
foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldM f a1 [x1, x2, ..., xm] = do
      a2 <- f a1 x1
      a3 <- f a2 x2
      ...
      f am xm
```

```haskell
filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
filterM _ [] = return []
filterM p (x:xs) = do
      flag <- p x
      ys   <- filterM p xs
      return (if flag then x:ys else ys)
```
