module Sort where

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x < y = x:y:ys
                | otherwise = y:insert x ys

insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)

swaps :: Ord a => [a] -> [a]
swaps [] = []
swaps [x] = [x]
swaps (x:y:t) | x > y = y : swaps (x:t)
              | otherwise = x : swaps (y:t)

fix :: Eq a => (a->a) -> a -> a
fix f x = if x == x' then x else fix f x'
  where x' = f x

bubbleSort :: Ord a => [a] -> [a]
bubbleSort xs = fix swaps xs

bubbleSort'' xs = bubbleSort'' initialElements ++ [lastElement]
  where swappedxs       = swaps xs
        initialElements = init swappedxs
        lastElement     = last swappedxs


delete _ [] = []
delete x (l:ls) | x==l = ls
                | otherwise = l:delete x ls

selectionSort [] = []
selectionSort xs = min : selectionSort xs'
  where min = minimum xs
        xs' = delete min xs


-- naive
quickSort [] = []
quickSort (x:xs) = quickSort min ++ [x] ++ quickSort max
  where min = filter (<x) xs
        max = filter (>=x) xs

-- better
quickSort' [] = []
quickSort' [x] = [x]
quickSort' (x:xs) = quickSort' min ++ [x] ++ quickSort' max
  where (min,max) = filterSplit (<x) xs

filterSplit :: (a->Bool) -> [a] -> ([a],[a])
filterSplit pred [] = ([],[])
filterSplit pred (x:xs)
  | pred x = (x:l, r)
  | otherwise = (l, x:r)
  where (l,r) = filterSplit pred xs


merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x < y = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]  -- this is necessary, or it never halts
mergeSort xs = merge (mergeSort x1) (mergeSort x2)
  where (x1,x2)  = halve xs
        halve xs = (take l xs, drop l xs)
        l        = (length xs) `div` 2

