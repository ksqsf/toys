{-# LANGUAGE DeriveFunctor, FlexibleInstances, Arrows, TupleSections #-}

import Control.Arrow hiding (Kleisli(..))
import Control.Category hiding (Kleisli(..))
import Prelude hiding (id, (.))

newtype SF a b = SF {runSF :: [a] -> [b]} deriving Functor

instance Applicative (SF b) where
  pure b = SF $ \as -> map (const b) as
  -- { f (a -> b) } <*> { f a }
  -- SF c (a -> b) <*> SF c a :: SF c b
  (SF f) <*> (SF x) = SF $ \cs -> zipWith ($) (f cs) (x cs)

instance {-# OVERLAPPABLE #-} (Arrow arr) => Functor (arr a) where
  -- f :: b -> c
  -- ab :: arr a b
  -- res :: arr a c
  fmap f ab = proc c -> do
                b <- ab -< c
                returnA -< f b

instance {-# OVERLAPPABLE #-} (Arrow arr) => Applicative (arr a) where
  pure x = arr (\a -> x)
  (<*>) f x = proc a -> do
                bc <- f -< a
                b <-x-<a
                returnA -< bc b

newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }

class Applicative f => Static f where
  delay :: Kleisli f a b -> Kleisli f () (a -> b)

instance Static f => Category (Kleisli f) where
  id = Kleisli pure
  -- bfc :: Kleisli f b c
  -- afb :: Kleisli f a b
  -- res :: Kleisli f a c
  (.) bfc afb = Kleisli $ \a -> let fab = (runKleisli $ delay afb) ()
                                    fbc = (runKleisli $ delay bfc) ()
                                in fbc <*> (fab <*> pure a)

instance Static f => Arrow (Kleisli f) where
  arr f = Kleisli $ \a -> pure (f a)
  first bfc = Kleisli $ \(b,d) -> let fbc = (runKleisli $ delay bfc) ()
                                  in (,d) <$> (fbc <*> pure b)
