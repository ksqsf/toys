{-# LANGUAGE ExistentialQuantification #-}
import Prelude hiding ((.))
import Control.Category hiding (id)

class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d

data PComp p q x y = forall z. PComp (p x z) (q z y)
instance (Profunctor p, Profunctor q) => Profunctor (PComp p q) where
  dimap nat1 nat2 (PComp pxz qzy) = PComp (dimap nat1 id pxz) (dimap id nat2 qzy)

class Category a => Arrow a where
  arr :: (x -> y) -> a x y
  (>>>) :: a x z -> a z y -> a x y
  f >>> g = g . f


