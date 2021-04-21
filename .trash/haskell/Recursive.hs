power :: Int -> Int -> Int
power 0 0 = 1
power _ 0 = 1
power x n = x * power x (n - 1)

power' :: Int -> Int -> Int
power' 0 0 = 1
power' _ 0 = 1
power' x n | odd n = let p = power' x ((n-1) `div` 2) in x * p * p
           | otherwise = let p = power' x (n `div` 2) in p * p


snoc :: a -> [a] -> [a]
snoc x [] = [x]
snoc x (y:ys) = y : snoc x ys


delete' :: Eq a => a -> [a] -> [a]
delete' a [] = []
delete' a (x:xs) | a == x    = delete' a xs
                 | otherwise = x : (delete' a xs)
-- delete' 1 [1,1..] should be []
-- why does haskell compute?


drop' :: Int -> [a] -> [a]
drop' n xs | n <= 0 = xs
drop' _ [] = []
drop' n (x:xs) = drop' (n-1) xs

fibStep :: (Eq a, Num a) => (a,a) -> (a,a)
fibStep (u,v) = (v, u+v)

fibPair :: (Eq a, Num a) => a -> (a,a)
fibPair 0 = (0,1)
fibPair n = fibStep (fibPair (n-1))

fastFib :: (Eq b, Num b) => b -> b
fastFib = fst . fibPair

fibs :: (Enum b, Eq b, Num b) => b -> [b]
fibs n = map fastFib [1..n]

fibs' n = take n (map fst (iterate fibStep (0,1)))


