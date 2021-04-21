#!/usr/bin/env stack
-- stack runghc
module P1B where

input :: FilePath
input = "P1A.txt"

numbers :: IO [Int]
numbers = do
  contents <- readFile input
  return $ map read (lines contents)

fuel :: Int -> Int
fuel mass = 0 `max` ((mass `quot` 3) - 2)

total :: Int -> Int
total n = fuel n + addition
  where addition
          | fuel n == 0 = 0
          | otherwise   = total (fuel n)

main = numbers >>= \xs -> print $ sum $ map total xs
