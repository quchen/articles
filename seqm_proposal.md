Strictifying Monadic values
===========================

Half a year ago, Johan Tibell proposed adding a strict
equivalent to fmap. This discussion has ended in
bikeshedding, it was reopened, it ended in bikeshedding
again. This is a third attempt to get this through, with
individual options and no alternatives proposed, compiled
from the two more popular responses in the other threads.

Please do not propose alternative implementations here,
we've been through this twice already. Vote -1 to show
disagreement. I think this functionality should be in
the standard libraries in one way or another, regardless
of how it's named in the end.



1. Add a strict version of <$>, named <$!>.

  ```haskell
  infixl 4 <$!>

  (<$!>) :: Monad m => (a -> b) -> m a -> m b
  f <$!> m = do x <- m
                return $! f x
  {-# INLINE (<$!>) #-}
  ```

  This is closely related to fmap in terms of functionality,
  but requires a more restrictive Monad constraint.



2. Add a seqM function that evaluates the "contents" of a
  monadic action to WHNF.

  ```haskell
  seqM :: Monad m => m a -> m a
  seqM m = do x <- m
              return $! x
  ```

  This is less close to fmap, but allows building other
  strict operations (locally and as needed) based on it
  easier, for example

  ```haskell
   f <$!>  x = seqM ( f <$>  x)
  mf <*!> mx = seqM (mf <*> mx)
  mf <*!  mx = seqM (mf <*  mx)
  mf  *!> mx = seqM (mf  *> mx)
  ```

  If these operators then turn out to be used (and locally
  reinvanted) very often by different people, reconsider
  adding them as well.



A voting period of two weeks (until 09-05-2014) should be
sufficient to allow everyone to join in.