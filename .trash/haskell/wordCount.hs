module Main where

import Data.Sort
import qualified Data.Map.Strict as M
wordCount :: String -> [(String, Int)]
wordCount str = reverse $ sortBy (\a b -> compare (fst a) (fst b)) $ M.toList $ foldl (\m x -> M.insert x (1 + M.findWithDefault 0 x m) m) M.empty (words str)
