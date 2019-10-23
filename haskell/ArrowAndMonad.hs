{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances,Arrows #-}
import Control.Monad
import Control.Applicative
import Control.Arrow hiding (Kleisli)
import Control.Category
import Prelude hiding ((.), id)

newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }

instance Monad m => Category (Kleisli m) where
  id = Kleisli return
  (Kleisli f) . (Kleisli g) = Kleisli (f <=< g)

instance Monad m => Arrow (Kleisli m) where
  arr f = Kleisli (return . f)
  first (Kleisli f) = Kleisli (\ ~(b,d) -> f b >>= \c -> return (c,d))

-- class Arrow a => ArrowApply (a :: * -> * -> *) where
--   app :: a (a b c, b) c
  
instance Monad m => ArrowApply (Kleisli m) where
  app = Kleisli (\(Kleisli f, x) -> f x)

-- instance ArrowApply a => Monad (a ()) where
--   return x = arr $ \_ -> x
--   x >>= f = proc () -> do
--               b <-x-< ()
--               ac <- arr f -< b
--               c <- ac -<< ()
--               returnA -< c
