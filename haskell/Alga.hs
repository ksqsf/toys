#!/usr/bin/env stack
{-# LANGUAGE TypeFamilies #-}
module AlgaTest where

import Data.Set (Set, singleton, union, elems, fromAscList)
import qualified Data.Set as Set (empty)

class Graph g where
  type Vertex g
  empty :: g
  vertex :: Vertex g -> g
  overlay :: g -> g -> g
  connect :: g -> g -> g

edge :: Graph g => Vertex g -> Vertex g -> g
edge x y = connect (vertex x) (vertex y)

vertices :: Graph g => [Vertex g] -> g
vertices = foldr overlay empty . map vertex

clique :: Graph g => [Vertex g] -> g
clique = foldr connect empty . map vertex

data Relation a = R { domain :: Set a, relation :: Set (a, a) } deriving (Show, Eq)

instance Ord a => Graph (Relation a) where
  type Vertex (Relation a) = a
  empty = R Set.empty Set.empty
  vertex x = R (singleton x) Set.empty
  overlay x y = R (domain x `union` domain y) (relation x `union` relation y)
  connect x y = R (domain x `union` domain y) (relation x `union` relation y `union`
                                                fromAscList [(a,b) | a<-elems (domain x),
                                                                     b<-elems (domain y)])

instance (Ord a, Num a) => Num (Relation a) where
  fromInteger = vertex . fromInteger
  (+) = overlay
  (*) = connect
  signum = const empty
  abs =id
  negate =id

data Embed a = Empty
             | Vertex a
             | Overlay (Embed a) (Embed a)
             | Connect (Embed a) (Embed a)

instance Graph (Embed a) where
  type Vertex (Embed a) = a
  empty = Empty
  vertex = Vertex
  overlay = Overlay
  connect = Connect

fold :: Graph g => Embed (Vertex g) -> g
fold Empty = empty
fold (Vertex x) = vertex x
fold (Overlay x y) = overlay (fold x) (fold y)
fold (Connect x y) = connect (fold x) (fold y)

instance Ord a => Eq (Embed a) where
  x == y = fold x == (fold y :: Relation a)
