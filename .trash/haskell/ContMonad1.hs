{-# LANGUAGE DeriveFunctor #-}

import Control.Applicative
import Control.Monad.Identity

data Free f a = Pure a
              | Free (f (Free f a)) a

newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r } deriving Functor

instance Monad (ContT r m) where
  return x = ContT $ \k -> k x
  ca >>= acb = ContT $ \br -> runContT ca (\a -> runContT (acb a) (\b -> br b))

instance Applicative (ContT r m) where
  pure = return
  (<*>) = ap

class Monad m => MonadCont m where
  callCC :: ((a -> m b) -> m a) -> m a

instance MonadCont (ContT r m) where
  callCC f = ContT $ \ar -> runContT (f (\a -> ContT $ \_ -> ar a)) ar

fact_cps :: Int -> ContT r Identity Int
fact_cps 0 = return 1
fact_cps n = do
  a <- fact_cps (n-1)
  b <- callCC $ \exit ->
                  if a > 10000 then exit 0 else return a
  return (b*n)
