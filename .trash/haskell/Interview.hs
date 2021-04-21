{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

nil = undefined

data Nil
data Cons x xs

class First list x | list -> x
instance First Nil Nil
instance First (Cons x more) x

class ListConcat a b c | a b -> c
instance ListConcat Nil x x
instance (ListConcat as bs cs) => ListConcat (Cons a as) bs (Cons a cs)

-- Concatenate all lists in a list
class ListConcatAll ls l | ls -> l
instance ListConcatAll Nil Nil
instance (ListConcat chunk acc result,
          ListConcatAll rest acc)
         => ListConcatAll (Cons chunk rest) result

-- Is any element of this list True?
class AnyTrue list t | list -> t
instance AnyTrue Nil False
instance AnyTrue (Cons True more) True
instance (AnyTrue list t) => AnyTrue (Cons False list) t

data True
data False

class Not b1 b | b1 -> b
instance Not False True
instance Not True False

class Or b1 b2 b | b1 b2 -> b
instance Or True True True
instance Or True False True
instance Or False True True
instance Or False False False

data Z
data S n

type N0 = Z
type N1 = S N0
type N2 = S N1
type N3 = S N2
type N4 = S N3
type N5 = S N4
type N6 = S N5
type N7 = S N6
type N8 = S N7

-- Equality
class PeanoEqual a b t | a b -> t
instance PeanoEqual Z Z True
instance PeanoEqual (S a) Z False
instance PeanoEqual Z (S b) False
instance (PeanoEqual a b t) => PeanoEqual (S a) (S b) t

-- Comparison
class PeanoLT a b t | a b -> t
instance PeanoLT Z Z False
instance PeanoLT (S x) Z False
instance PeanoLT Z (S x) True
instance (PeanoLT a b t) => PeanoLT (S a) (S b) t

-- Absolute difference
class PeanoAbsDiff a b c | a b -> c
instance PeanoAbsDiff Z Z Z
instance PeanoAbsDiff Z (S b) (S b)
instance PeanoAbsDiff (S a) Z (S a)
instance (PeanoAbsDiff a b c) => PeanoAbsDiff (S a) (S b) c

-- Integers from n-1 to 0
class Range n xs | n -> xs
instance Range Z Nil
instance (Range n xs) => Range (S n) (Cons n xs)

-- class LegalCompare t | -> t
--   where legalCompare :: t
-- instance (PeanoEqual (S Z) (S Z) t)
--   => LegalCompare t

-- class illegalcompare t | -> t
--   where illegalcompare :: t
-- instance (peanoequal true (cons z false) t) => illegalcompare t

--- Note: the type of illegalCompare is illegalCompare :: PeanoEqual True (Cons Z False) t => t
--- This is an unqualified formula, which means there might be a model in which this formula is false.
--- In turn, it means this sentence is not derivable in our axiomatic system.

-- f accepts a and returns r
class Apply f a r | f a -> r

-- Conj1 is a partial-applied function that prepends an element to it.
-- Note how we transformed a function to a term!
data Conj1 list
instance Apply (Conj1 list) x (Cons x list)

-- Map f over a list
class Map f xs ys | f xs -> ys
instance Map f Nil Nil
instance (Apply f x y, Map f xs ys) => Map f (Cons x xs) (Cons y ys)

-- Map f over list and concatenate results together
class MapCat f xs zs | f xs -> zs
instance MapCat f Nil Nil
instance (Map f xs chunks, ListConcatAll chunks ys) => MapCat f xs ys

-- Filter a list with an Apply-able predicate function
class AppendIf pred x ys zs | pred x ys -> zs
instance AppendIf True x ys (Cons x ys)
instance AppendIf False x ys ys

class Filter f xs ys | f xs -> ys
instance Filter f Nil Nil
instance (Apply f x t,
          Filter f xs ys,
          AppendIf t x ys zs)
  => Filter f (Cons x xs) zs

data Queen x y
data Queen1 x                   -- Construct a queen from a `y'
instance Apply (Queen1 x) y (Queen x y)

-- A list of queens in row x with y from 0 to n
class QueensInRow n x queens | n x -> queens
instance (Range n ys, Map (Queen1 x) ys queens)
  => QueensInRow n x queens

-- Does queen a threaten queen b?
class Threatens a b t | a b -> t
instance (PeanoEqual ax bx xeq,
          PeanoEqual ay by yeq,
          Or xeq yeq xyeq,
          PeanoAbsDiff ax bx dx,
          PeanoAbsDiff ay by dy,
          PeanoEqual dx dy deq,
          Or xyeq deq res)
  => Threatens (Queen ax ay) (Queen bx by) res



-- Partial application of threatens
data Threatens1 a
instance (Threatens a b t)
  => Apply (Threatens1 a) b t

-- Is queen b compatible with all queen as?
class Safe config queen t | config queen -> t
instance (Map (Threatens1 queen) config m1,
          AnyTrue m1 t1,
          Not     t1 t2)
  => Safe config queen t2

data Safe1 config
instance (Safe config queen t)
  => Apply (Safe1 config) queen t

class AddQueen n x c cs | n x c -> cs
instance (QueensInRow n x candidates,
          Filter (Safe1 c) candidates filtered,
          Map (Conj1 c) filtered cs)
  => AddQueen n x c cs

data AddQueen2 n x
instance (AddQueen n x c cs)
  => Apply (AddQueen2 n x) c cs

class AddQueenToAll n x cs cs' | n x cs -> cs'
instance (MapCat (AddQueen2 n x) cs cs')
  => AddQueenToAll n x cs cs'

class AddQueensIf pred n x cs cs' | pred n x cs -> cs'
instance AddQueensIf False n x cs cs
instance (AddQueenToAll n x cs cs2,
          AddQueens n (S x) cs2 cs')
  => AddQueensIf True n x cs cs'

class AddQueens n x cs cs' | n x cs -> cs'
instance (PeanoLT x n pred,
          AddQueensIf pred n x cs cs')
  => AddQueens n x cs cs'

class Solution n c | n -> c where
  solution :: n -> c
instance (AddQueens n Z (Cons Nil Nil) cs,
          First cs c)
  => Solution n c where solution = nil
