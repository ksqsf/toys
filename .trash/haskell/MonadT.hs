import Control.Monad.State
import Control.Monad.Identity
-- import Control.Monad.Maybe

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Monad m => Monad (MaybeT m) where
  return x = MaybeT $ return (Just x)
  MaybeT ma >>= k = MaybeT $ do
    result <- ma
    case result of
      Nothing -> return Nothing
      Just a -> runMaybeT (k a)

instance Monad m => Applicative (MaybeT m) where
  pure = return
  (<*>) = ap

instance Monad m => Functor (MaybeT m) where
  fmap f x = do
    a <- x
    MaybeT $ return $ Just (f a)

-- type MaybeIOString = MaybeT IO String

safeHead :: [a] -> MaybeT Identity a
safeHead [] = MaybeT $ Identity Nothing
safeHead (x:xs) = MaybeT $ Identity $ Just x

