-- import Test.HUnit

-- main :: IO ()
-- main = do
--   runTestTT test1
--   return ()

-- foo _ = (1, True)
-- test1 = TestCase (assertEqual "for (foo 3)" (1, True) (foo 3))

import Test.QuickCheck

import Data.List (sort)

prop_even x y = even (x+y) == (even x == even y)
prop_reverseUnit x = reverse [x] == [x]
prop_reverseConcat xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs
prop_reverseTwice xs = (reverse.reverse) xs == xs
prop_reverseMap f xs = (map f.reverse) xs == (reverse.map f) xs

ordered :: Ord a => [a] -> Bool
ordered [] = True
ordered [x] = True
ordered (x:y:ys) = x<=y && ordered (y:ys)

prop_ordered xs = ordered $ sort xs

prop_headMin' :: Ord a => [a] -> Property
prop_headMin' xs = not (null xs) ==> head (sort xs) == minimum xs


