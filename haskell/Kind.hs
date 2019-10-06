{-# LANGUAGE GADTs, KindSignatures, FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
data T :: * -> * where
  NIL :: T a
  CONS :: a -> T a -> T a

data AbsTree k a = Leaf a | Node (k (AbsTree k a))

data Tree :: (* -> *) -> * -> * where
  L :: a -> Tree k a
  N :: k (Tree k a) -> Tree k a

type RoseTree a = Tree [] a

instance Show a => Show (RoseTree a) where
  show (L a) = show a
  show (N tree) = show tree

test :: RoseTree Int
test = N [N [L 5, L 8, N [L 1, L 2]], N [L 3]]

test2 = N (NIL)

data F f = MkF (f Int)
data Ty f a = MkTy (f a)
-- Ty :: forall {k}. (k -> *) -> k -> *

type T1 = Ty Maybe Int
type T2 = Ty F Maybe

