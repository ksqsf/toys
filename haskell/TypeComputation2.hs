{-# LANGUAGE KindSignatures, GADTs, TypeOperators, DataKinds, UndecidableInstances, StandaloneDeriving, ExistentialQuantification, TypeFamilies #-}

data Nat = Z | S Nat deriving (Eq, Show)

type family Plus (a :: Nat) (b :: Nat) :: Nat
type instance Plus Z k = k
type instance Plus (S m) k = Plus m (S k)

data Vec a (n :: Nat) where
  Nil :: Vec a Z
  Cons :: a -> Vec a n -> Vec a (S n)

deriving instance Show a => Show (Vec a n)

vhead :: Vec a (S n) -> a
vhead (Cons a v) = a

vtail :: Vec a (S n) -> Vec a n
vtail (Cons x xs) = xs

toList :: Vec a n -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs

data SNat (n :: Nat) where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

fromList :: SNat n -> [a] -> Vec a n
fromList SZ [] = Nil
fromList (SS n) (x:xs) = Cons x (fromList n xs)
fromList _ _ = error "size not matched"
