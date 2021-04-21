#!/usr/bin/env stack
{- stack runghc -}
module P1A where

input :: FilePath
input = "P1A.txt"

numbers :: IO [Int]
numbers = do
  contents <- readFile input
  return $ map read (lines contents)

fuel :: Int -> Int
fuel mass = 0 `max` ((mass `quot` 3) - 2)

main :: IO ()
main = do
  xs <- numbers
  print $ sum $ map fuel xs
