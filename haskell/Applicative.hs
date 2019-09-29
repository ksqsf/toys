{-# LANGUAGE DeriveFunctor #-}
import Control.Applicative

(<<*>>) :: (Applicative f1, Applicative f2) => f1 (f2 (a -> b)) -> f1 (f2 a) -> f1 (f2 b)
(<<*>>) = liftA2 (<*>)

data Identity a = Identity { runIdentity :: a } deriving (Show, Read, Functor)

instance Applicative Identity where
  pure = Identity
  (<*>) f x = Identity $ (runIdentity f) (runIdentity x)

ex1 = (+) <$> (Identity 1) <*> (Identity 2)

-- Identity cannot be defined as Alternative
