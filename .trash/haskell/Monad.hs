{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
import Prelude hiding ((>>=), Monad, Just, Nothing, Maybe, return)

class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  -- bind
  
  return :: a -> m a
  -- return

  (>>) :: m a -> m b -> m b
  m >> k = m >>= \_ -> k
  fail :: String -> m a
  fail s = error s


newtype Identity a = Identity { runIdentity :: a } deriving (Functor)

instance Applicative Identity where
  pure a = Identity a
  (<*>) (Identity f) (Identity a) = Identity (f a)

instance Monad Identity where
  return = pure
  (>>=) (Identity a) k = k a


-- This monad can hold values, and handle exceptions during computation.
data Maybe a = Just a | Nothing deriving Functor

instance Applicative Maybe where
  pure = Just
  Just f <*> Just a = Just (f a)
  
instance Monad Maybe where
  return = Just
  Just a >>= k = k a
  Nothing >>= _ = Nothing
  fail _ = Nothing


instance Monad [] where
  return x = [x]
  xs >>= f = concatMap f xs
  fail _ = []

plus :: Num b => [b] -> [b] -> [b]
plus xs ys = do
  x <- xs
  y <- ys
  return (x+y)
-- [x+y|x<-xs, y<-ys]

class Monad m => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a

instance MonadPlus m => Monoid (m a) where
  mempty = mzero
  mappend = mplus



