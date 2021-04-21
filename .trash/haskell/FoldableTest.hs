{-# LANGUAGE DeriveFoldable #-}
import Data.Foldable
import Data.Monoid

data Tree a = Leaf a | Node (Tree a) a (Tree a)
  deriving (Show)

instance Foldable Tree where
  foldMap f (Leaf x) = f x
  foldMap f (Node l n r) = foldMap f l `mappend` f n `mappend` foldMap f r

tree = (Node (Leaf 1) 2 (Node (Leaf 3) 4 (Leaf 5)))

flatten :: Tree a -> [a]
flatten = foldMap (: [])

all :: Foldable t => (a -> Bool) -> t a -> Bool
all p = getAll . foldMap (All . p)

find :: Foldable t => (a -> Bool) -> t a -> Maybe a
find p = getFirst . foldMap (\x -> First (if p x then Just x else Nothing))
