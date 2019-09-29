data Tree a = Leaf a | Branch (Tree (a, a)) deriving Show
-- This tree must be a full binary tree.
-- Leaf a
-- Branch Leaf (a,a)
-- Branch Branch Leaf ((a,a), (a,a))
-- ...

instance Functor Tree where
  fmap f (Leaf l) = Leaf (f l)
  -- br     :: Tree (a,a)
  -- f      :: a -> b
  -- fmap f :: Tree a -> Tree b
  -- g      :: (a,a) -> (b,b)
  -- fmap g :: Tree (a,a) -> Tree (b,b)
  fmap f (Branch br) = let g = \(a1,a2) -> (f a1, f a2)
                       in Branch (fmap g br)
