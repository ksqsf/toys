{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures, FlexibleContexts #-}

data T a b = Q | N a b

data Tree a = Leaf a | Node (Tree a) (Tree a)

data Choice = I Int
            | C Char
            | B Choice Bool
            | S Choice
            deriving Show

instance (Eq a, Eq b) => Eq (T a b) where
  Q == Q = True
  (N x1 y1) == (N x2 y2) = x1 == x2 && y1 == y2
  _ == _ = False

instance Eq Choice where
  (I i1) == (I i2) = i1 == i2
  (C c1) == (C c2) = c1 == c2
  (B c1 b1) == (B c2 b2) = c1 == c2 && b1 == b2
  (S c1) == (S c2) = c1 == c2
  _ == _ = False

instance (Eq a => Eq (Tree a)) where
  Leaf a1 == Leaf a2 = a1 == a2
  (Node n1 n2) == (Node n3 n4) = n1 == n3 && n2 == n4
  _ == _ = False

-- All of them are boilerplate!

data U = U deriving (Show, Eq) -- unit
data a :*: b = a :*: b deriving (Show, Eq)
data a :+: b = L a | R b deriving (Show, Eq)

type MyBool = U :+: U
data Three = One | Two | Three
type AlgThree = (U :+: U) :+: U

class Generic a where
  type Rep a :: *
  from :: a -> Rep a
  to   :: Rep a -> a

instance Generic Bool where
  type Rep Bool = U :+: U
  from False = L U
  from True = R U
  to (L U) = False
  to (R U) = True

data List a = Nil | Cons a (List a)

instance Generic (List a) where
  type Rep (List a) = U :+: (a :*: (List a))
  from Nil = L U
  from (Cons x xs) = R (x :*: xs)
  to (L U) = Nil
  to (R (x :*: xs)) = Cons x xs


class GEq a where
  geq :: a -> a -> Bool
  default geq :: (Generic a, GEq (Rep a)) => a -> a -> Bool
  geq = defaultEq

defaultEq :: (Generic a, GEq (Rep a)) => a -> a -> Bool
defaultEq x y = geq (from x) (from y)

instance (GEq a, GEq b) => GEq (a :+: b) where
  geq (L a1) (L a2) = geq a1 a2
  geq (R b1) (R b2) = geq b1 b2
  geq _ _ = False

instance (GEq a, GEq b) => GEq (a :*: b) where
  geq (a1 :*: b1) (a2 :*: b2) = geq a1 a2 && geq b1 b2

instance GEq U where
  geq U U = True

instance GEq Bool

instance ((GEq a, Generic a) => GEq (List a))
