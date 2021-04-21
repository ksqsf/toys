{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE FlexibleInstances #-}

module Lib where

import Data.Map as Map hiding (map)
import Data.Set as Set hiding (map)

type P a = Set.Set a
type k :-> v = Map.Map k v

-- Preorder: relation is reflexible and transitive
-- Equivalence: preorder + symmetric
-- Every preorder induces a natural equivalence relation

-- Partial Order: a preorder + antisymmetric
-- poset, or partial ordered set
-- Antisymmetry: aRb and bRa -> a=b

class PartialOrder t where
  (⊑) :: t -> t -> Bool

instance PartialOrder Integer where
  a ⊑ b = a <= b

-- Total order: total, transitive, and antisymmetric
-- Total: for any pairs of a and b, aRb or bRa



-- Meet semilattices
-- any two elements a and b have a greatest lower bound, a⊓b

-- Join semilattices
-- any two elements a and b have a least upper bound, a⊔b

-- Lattice = Meet + Join semilattices
class PartialOrder t => Lattice t where
  (⊔) :: t -> t -> t
  (⊓) :: t -> t -> t

instance Lattice Integer where
  a ⊔ b = if a <= b then a else b
  a ⊓ b = if a <= b then b else a

-- Bounded lattices
-- A lattice is bounded, if there exists a mamimum element (top or ⊤),
-- and a minimum element (bottom or ⊥)

class Lattice t => BoundedLattice t where
  bot :: t
  top :: t

-- Complete Lattices
-- if every subset S of L has both a least upper bound and a greatest lower bound

-- Monotonic functions
-- Given posets (X,⊑X) and (Y,⊑Y), a function
-- f:X→Y is monotonic or order-preserving if x⊑Xx′ implies
-- f(x)⊑Yf(x′).

-- Member-wise function: f: X -> Y, S ⊆ X, f.S = {f(x) | x ∈ S}
-- 
-- Scott-continuous: Given lattices (X, Rx), (Y, Ry), f: X->Y, ∀ S ⊆ X, f(sup(S)) = sup(f.S)
-- also monotonic

-- Fixed points
-- x = f(x)

-- Regions
-- Fix(f), Asc(f), Desc(f)
-- lfp(f) = inf(Fix(f)), least fixed point
-- gfp(f) = sup(Fix(f)), greatest fixed point

-- Kleene chain
--
-- Given a monotonic function f: L->L on a lattice (L,⊑), the Kleene
-- chain starting at the point x∈L is the set K(x):
-- K(x) = {f^i(x) | i >= 0}
kleene :: (BoundedLattice t) => (t -> t) -> [t]
kleene f = bot:(map f (kleene f))

-- Kleene's fixed point theorem
-- 如果 (L,<=) 是完全格，函数 f:L->L 是连续的，则 lfp(f) = sup(K(⊥))
-- 进一步，对于有限高度的格，存在自然数 n 使得 lfp(f) = f^n(⊥)
-- 
-- 这个事实可以用来计算不动点

stable :: Eq a => [a] -> a
stable (x:fx:tl) | x == fx = x
                 | otherwise = stable (fx:tl)

lfp :: (BoundedLattice t, Eq t) => (t -> t) -> t
lfp f = stable (kleene f)

-- Pointed sets
data Pointed a = PTop
               | PBot
               | PEl a
               deriving Show

instance PartialOrder a => PartialOrder (Pointed a) where
  PBot ⊑ x = True
  x ⊑ PTop = True
  (PEl a) ⊑ (PEl b) = a ⊑ b

-- Flat lattice
-- Given a set S, S can be lifted into a flat lattice
-- 相当于一个自由结构
data Flat a = Top
            | Bot
            | El a

instance (Eq a) => PartialOrder (Flat a) where
  Bot ⊑ x = True
  x ⊑ Top = True
  (El a) ⊑ (El b) = a == b

instance (Eq a) => Lattice (Flat a) where
  Bot ⊔ x = x
  x ⊔ Bot = x
  Top ⊔ x = Top
  x ⊔ Top = Top
  (El a) ⊔ (El b) | a == b    = El a
                  | otherwise = Top
  Bot ⊓ x = Bot
  x ⊓ Bot = Bot
  Top ⊓ x = x
  x ⊓ Top = x
  (El a) ⊓ (El b) | a == b    = El a
                  | otherwise = Bot

instance (Eq a) => BoundedLattice (Flat a) where
  bot = Bot
  top = Top

-- Partial orders over sums
-- 有一堆 poset，可以定义 poset 和上的偏序
-- x⊑x′ iff x⊑ix′ for some i.
-- 注：不一定是格，需要 pointed

-- Lattices over Cartesian products
-- 也是自然的

instance (PartialOrder a, PartialOrder b) => PartialOrder (a,b) where
  (x,y) ⊑ (x',y') = x ⊑ x' && y ⊑ y'

instance (Lattice a, Lattice b) => Lattice (a,b) where
 (x,y) ⊔ (x',y') = (x ⊔ x', y ⊔ y')
 (x,y) ⊓ (x',y') = (x ⊓ x', y ⊓ y')

instance (BoundedLattice a, BoundedLattice b) => 
         BoundedLattice (a,b) where
 bot = (bot,bot)
 top = (top,top)

-- Partial order over sequences
-- 完全同理
instance (PartialOrder a) => PartialOrder [a] where
  []     ⊑ []       = True
  (x:xl) ⊑ (x':xl') = (x ⊑ x') && (xl ⊑ xl')
  _      ⊑ _        = False -- 长度不同

instance (Lattice a) => Lattice (Pointed [a]) where
  PBot ⊔ x = x
  x ⊔ PBot = x

  PTop ⊔ x = PTop
  x ⊔ PTop = PTop 

  (PEl v) ⊔ (PEl v') = PEl [ x ⊔ x' | x <- v | x' <- v' ]

  PBot ⊓ x = PBot
  x ⊓ PBot = PBot

  PTop ⊓ x = x
  x ⊓ PTop = x

  (PEl v) ⊓ (PEl v') = PEl [ x ⊓ x' | x <- v | x' <- v' ]

instance (Lattice a) => BoundedLattice (Pointed [a]) where 
  bot = PBot
  top = PTop

-- Inclusion lattices
-- Given a set S, the subsets of S form a lattice under inclusion (P(S),⊆)

-- Preorder over posets
-- Given a partial order (X,⊑), we can defined the preordered set (P(X),⊑')
-- S⊑'S' <=> for each x ∈ S, there exists x' ∈ S' s.t. x ⊑ x'
--
-- This is not partial order because {1,2,3}⊑'{3} and {3}⊑'{1,2,3}
-- but {1,2,3}≠{3}


-- Partial orders from preorders
-- Lift a partial order to equivalence classes of the preorder
--
-- Given a preordered set (X,<=), the natural partial order over equivalence classes is the poset (X/=,⊑),
-- where a=b <=> a<=b and b<=a


-- Lattices over functions
-- Given a lattice (Y,⊑) and a set X, the natural point-wise lattice is the lattice (X->Y,⊑')
--
-- f <=' g <=> f(x) <= g(x) forall x
-- (f V g)(x) = f(x) V g(x)
-- (f ^ g)(x) = f(x) ^ g(x)

instance (Ord k, PartialOrder v) => PartialOrder (k :-> v) where
  f ⊑ g = isSubmapOfBy (⊑) f g

instance (Ord k, Lattice v) => Lattice (k :-> v) where
  f ⊔ g = unionWith (⊔) f g
  f ⊓ g = unionWith (⊓) f g

instance (Ord k, BoundedLattice v) => BoundedLattice (k :-> v) where
  bot = Map.empty
  top = top

