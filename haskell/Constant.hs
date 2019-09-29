{-# LANGUAGE InstanceSigs #-}
newtype Constant a b = Constant { getConstant :: a }

-- Applicative

instance Functor (Constant a) where
  fmap :: (b -> c) -> Constant a b -> Constant a c
  fmap f b = Constant $ getConstant b

instance Monoid a => Applicative (Constant a) where
  pure :: b -> Constant a b
  pure _ = Constant mempty

  (<*>) :: Constant a (x -> y) -> Constant a x -> Constant a y
  Constant x <*> Constant y = Constant (x `mappend` y)

instance Foldable (Constant a) where
  foldMap :: Monoid m => (a1 -> m) -> Constant a a1 -> m
  foldMap f x = mempty

instance Traversable (Constant a) where
  traverse :: Applicative f => (a1 -> f b) -> Constant a a1 -> f (Constant a b)
  traverse f (Constant x) = pure (Constant x)
