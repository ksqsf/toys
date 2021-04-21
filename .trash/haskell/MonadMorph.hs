{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

import Control.Category
import Data.Functor.Identity
import Control.Monad.Morph
import Prelude hiding ((.), id)
import Control.Monad.Trans.Class

-- newtype m ~~> n = MonadMorph { runMonadMorph :: forall a . m a -> n a }

-- instance Category (~~>) where
--   id = MonadMorph $ \x -> x
--   (.) (MonadMorph f) (MonadMorph g) = MonadMorph (f.g)

type m ~~> n = forall a. m a -> n a

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }
type State s a = StateT s Identity a
state :: (s -> (a, s)) -> State s a
state f = StateT $ \s -> return $ f s

instance Functor m => Functor (StateT s m) where
  fmap ab fa = StateT $ \s -> let asbs = \(a,s) -> (ab a, s)
                                  fasbs = fmap asbs
                              in fasbs (runStateT fa s)

instance Monad m => Applicative (StateT s m) where
  pure x = StateT $ \s -> pure (x, s)
  -- fab :: s -> m (a -> b)
  -- fa  :: s -> m (a,s)
  -- res :: s -> m (b,s)
  fab <*> fa = StateT $ \s -> do (ab, s1) <- runStateT fab s
                                 (a, s2) <- runStateT fa s1
                                 pure (ab a, s2)

instance Monad m => Monad (StateT s m) where
  return = pure
  -- ma :: StateT s m a :: s -> m (a,s)
  -- f :: a -> StateT s m b
  ma >>= f = StateT $ \s -> do (a, s') <- runStateT ma s
                               (b, s'') <- runStateT (f a) s'
                               return (b, s'')

instance MonadTrans (StateT s) where
  lift m = StateT $ \s -> m >>= \a -> return (a, s)

instance Lifting Monad (StateT s) where
  lifting = Sub Dict

instance MFunctor (StateT s) where
  hoist nat m = StateT $ \s -> nat (runStateT m s)

