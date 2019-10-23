{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

import GHC.Types (Constraint)

newtype Compose f g a = Compose { getCompose :: f (g a) }
newtype (:.:) f g p = Comp1 { unComp1 :: f (g p) }


data Dict (p :: Constraint) where
  Dict :: p => Dict p

instance Show (Dict p) where
  show Dict = "Dict"

newtype p :- q = Sub (p => Dict q)

-- instance Show (p :- q) where
--   showPrec d _ = showParen (d > 10) $ showString "Sub Dict"
refl :: a :- a
refl = Sub Dict

(\\) :: p => ((q => r) -> (p :- q) -> r)
r \\ (Sub Dict) = r

trans :: (b :- c) -> (a :- b) -> (a :- c)
trans f g = Sub $ (Dict \\ f) \\ g

