import Data.List (delete)

permutation :: Eq a => [a] -> [[a]]
permutation [] = [[]]
permutation xs = [y : ys | y <- xs, ys <- permutation (delete y xs)]
