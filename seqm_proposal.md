Edit: This proposal was implemented via `<$!>` in May 2014
      ([Trac ticket][ticket]).

[ticket]: https://ghc.haskell.org/trac/ghc/ticket/9099



Strictifying Monadic values
===========================

Half a year ago, Johan Tibell proposed adding a strict
equivalent to fmap ([link][orig-proposal]). This discussion
got lost in details, it was reopened, it ended in
bikeshedding again. This is a third attempt to get
this through, with individual options and no alternatives
proposed, compiled from the two more popular responses
in the other threads.

Please do not propose alternative implementations here,
we've been through this twice already. Vote ±1 on each
point to show (dis)agreement. I think this functionality
should be in the standard libraries in one way or
another, regardless of how it's named in the end.



1. Add a strict version of <$>, named <$!>.

  ```haskell
  infixl 4 <$!>

  (<$!>) :: Monad m => (a -> b) -> m a -> m b
  f <$!> m = do x <- m
                return $! f x
  ```

  This is closely related to fmap in terms of functionality,
  but requires a Monad constraint.

  This would allow defining

  ```haskell
  seqM m = id <$!> m
  ```

  if such a function is needed.



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



[orig-proposal]: http://www.haskell.org/pipermail/libraries/2013-November/021728.html