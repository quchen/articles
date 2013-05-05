-- This is the new GHC.Base, stripped of all comments (and Pragmas).

class  Functor f  where

    fmap :: (a -> b) -> f a -> f b

    (<$) :: a -> f b -> f a
    (<$) =  fmap . const



class Functor f => Applicative f where

    pure :: a -> f a

    (<*>) :: f (a -> b) -> f a -> f b

    (*>) :: f a -> f b -> f b
    (*>) a b = fmap (const id) a <*> b

    (<*) :: f a -> f b -> f a
    (<*) a b = fmap const a <*> b



class Applicative m => Monad m  where

    (>>=) :: m a -> (a -> m b) -> m b
    m >>= f = join $ fmap f m

    (>>) :: forall a b. m a -> m b -> m b
    (>>) = (*>)

    join :: m (m a) -> m a
    join m = m >>= id

    return :: a -> m a
    return = pure

    fail :: String -> m a
    fail s = error s
