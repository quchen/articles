sequence :: Monad m => [m a] -> m [a]
sequence = mapM id
sequence [a0, a1, ..., an] = do
      r0 <- a0
      r1 <- a1
      ...
      rn <- an
      return [r0, r1, ..., rn]



mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f = sequence . map f
mapM f [a0, a1, ..., an] = do
      r0 <- f a0
      r1 <- f a1
      ...
      rn <- f an
      return [r0, r1, ..., rn]



-- (\acc x -> newAcc) -> initialValue -> list -> result
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f`...) `f` xn
                            == f ( ... (f (f (f (f z x1) x2) x3) x4) ...) xn
{- Graphical version of foldl:
  :                            f
 / \                          / \
1   :         foldl f z      4   f
   / \           ==>            / \
  2   :                        3   f
     / \                          / \
    3   :                        2   f
       / \                          / \
      4  []                        1   z
-}



-- (\x acc -> newAcc) -> initialValue -> list -> result
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn `f` z)...))
                            == f x1 (f x2 (f x3 (f x4 ... (f xn z) ... )))
{- Graphical version of foldr:
  :                            f
 / \                          / \
1   :         foldr f z      1   f
   / \           ==>            / \
  2   :                        2   f
     / \                          / \
    3   :                        3   f
       / \                          / \
      4  []                        4   z
-}



-- foldM is like foldl, but the accumulator is monadic.
foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldM f a1 [x1, x2, ..., xm] = do
      a2 <- f a1 x1
      a3 <- f a2 x2
      ...
      f am xm



filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
filterM _ [] = return []
filterM p (x:xs) = do
      flag <- p x
      ys  <- filterM p xs
      return (if flag then x:ys else ys)
