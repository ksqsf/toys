import Data.List
import qualified Data.Set as Set

a1 :: [Int]
a1 = [1,3..15]

a2 :: [Int]
a2 = take 32 [0..]

dot :: [Int] -> [Int] -> Int
dot x y = sum $ zipWith (*) x y

gen :: Int -> [[Int]]
gen 0 = []
gen 1 = [[1], [0]]
gen n = let rest = gen (n-1) in (map (0:) rest) ++ (map (1:) rest)

-- {a.x | lower elements of x are 0 }
plus :: [Int] -> [Int]
plus a = let n = length a
             m = n `div` 2
         in sort $ [dot a (replicate (n-m) 0 ++ s) | s <- gen m]

-- b - {a.x | upper elements of x are 0}
minus :: [Int] -> Int -> [Int]
minus a b = let n = length a
                m = (n+1) `div` 2
            in sort $ [b - (dot a (s ++ replicate (n-m) 0)) | s <- gen m]

match :: [Int] -> [Int] -> Bool
match [] _ = False
match _ [] = False
match (a:as) (b:bs)
  | a == b = True
  | a > b  = match (a:as) bs
  | a < b  = match as (b:bs)

setsum :: [Int] -> Int -> Bool
setsum a b = match (plus a) (minus a b)

allsum :: [Int] -> [Int]
allsum [] = [0]
allsum (x:[]) = [0,x]
allsum (x:xs) = map (+x) (allsum xs) ++ allsum xs

allsum' :: [Int] -> [Int]
allsum' xs = nub $ sort $ allsum xs

ex0 = any (==False) $ map (setsum a1) [0..64]
ex1 = all (==True) $ map (setsum a2) [0..32]

-- Correctness:
-- If a pair of numbers add up to b, then there must exist a vector x such that a.x = b
-- If there is a vector x s.t. a.x=b, then it can be written as a.(x1+x2)=a.x1 + a.x2, and here the possibilities
-- of x1 and x2 are exhausted

-- Complexity: O(2^(n/2))

-- a -> f b
-- t a
-- t a -> f (t b)
