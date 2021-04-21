{-# LANGUAGE TypeSynonymInstances #-}
{-|
Module      : BFS
Description : Demonstration of BFS
Copyright   : (c) John Doe, 2019
License     : WTFPL
Maintainer  : whoever cares?
Stability   : experimental
Portability : POSIX

This module demonstrates how to write a breadth-first search in the
"functional" style. Why not DFS? Cuz it's so trivial! @wink wink@

BFS is the "standard" search method. It relies on the following
components:

0. Your notion of a "state".
1. An "initial" state.
2. A "goal" state.
3. A function that computes the set of "next" states.

BFS guarantees to find one of the optimal solutions, in the sense that
the total cost is minimized. However, the BFS algorithm presented here
in the "BFS" module fixes the cost of every transition to be 1, so
it's not very general, but it can be used to find a shortest path in a
graph.

Use haddock to generate doc. Ok I admit this is actually a testbed for
Haddock.

> haddock --html BFS.hs --hyperlinked-source --use-unicode --package-name wtf --package-version 0.0.1.0 --pretty-html --title=WTFLib

The haddock output looks pretty cool! <https://www.haskell.org/haddock/doc/html/markup.html Learn Haddock here.>
-}
module BFS where

-- | A general class that defines the problem.
class ProblemState a where
  -- | The initial state.
  initial :: a
  -- | The function that computes the next states.
  next    :: a -> [a]
  -- | The function that tests whether this state is a goal state.
  goal    :: a -> Bool

-- | A transition in the process of the problem solving.  It checks
-- the first element in the queue, and signals it if it's a solution.
-- Otherwise, it computes the next states, append them to the queue,
-- and continue inspecting older states.
bfsIter :: ProblemState a => [a] -> Maybe a
bfsIter []    = Nothing
bfsIter (x:xs)
  | goal x    = Just x
  | otherwise = bfsIter (xs ++ next x)

-- | Solve a problem with BFS.  Note this interface is extremely
-- rigid.  It forces you to give a complete description of your
-- problem statically.
--
-- Given a problem description like 'Collatz', invoke 'bfs' in the
-- following way:
--
-- >>> bfs :: Maybe Collatz
-- Just 1
bfs :: ProblemState a => Maybe a
bfs = bfsIter [initial]

-- | The Collatz conjecture.
data Collatz = C Int deriving Show

instance ProblemState Collatz where
  initial = C 1234
  next (C x)
    | even x = [C $ x `quot` 2]
    | odd  x = [C $ x * 3 + 1]
  goal (C 1) = True
  goal _     = False
