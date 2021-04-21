{-# LANGUAGE ExistentialQuantification #-}

data Day f g a = forall x y. Day (f x) (g y) ((x, y) -> a)

infixr 0 :~>
type :~> = Nat

bimap :: (f :~> h) (g :~> k) -> (Day f g :~> Day h k)
bimap nat1 nat2 (Day fx gy xya) = Day (nat1 fx) (nat2 gy) (xya)

class Functor f => DayApplicative f where
  eday :: (() -> a) -> f a
  muday :: Day f f a -> f a

