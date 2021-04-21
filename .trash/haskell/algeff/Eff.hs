module Eff where

-- | Eff represents a computation that can have any "effects", which
-- | must be handled by some handler.
data Eff a = Eff {  }

choose :: a -> a -> Eff a
choose a b = do
  b <- decide
  if b then a else b

example :: Eff (Int, Int)
example = do
  x <- chooseInt 1 3
  y <- chooseInt 3 4
  return (x, y)
