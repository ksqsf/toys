{-# LANGUAGE NoMonadFailDesugaring #-}

-- monad-par
import Control.Monad.Par
import Control.Monad.IO.Class
import Control.Monad

-- isPrime :: Integer -> Bool
-- isPrime 2 = True
-- isPrime p = p>1 && (all (\n ->p `mod` n /= 0) $ takeWhile (\n-> n*n <= p) [3, 5 ..])

-- generateKey :: Integer -> Integer -> Integer
-- generateKey x y = runPar $ do
--   [k1, k2] <- sequence [new, new]
--   fork $ put k1 (isPrime x)
--   fork $ put k2 (isPrime y)
--   p1 <- get k1
--   p2 <- get k2
--   return $ if p1 && p2 then x * y else 0

-- p1, p2 :: Integer
-- p1 = 2646507710984041
-- p2 = 1066818132868207
-- main = print $ generateKey p1 p2

parfib :: Integer -> Par Integer
parfib n
  | n <= 2 = return 1
  | otherwise = do
      x <- spawn $ parfib (n-1)
      y <- spawn $ parfib (n-2)
      x' <- get x
      y' <- get y
      return (x' + y')

main :: IO ()
main = do
  let ans = runPar $ parfib 40
  print ans
  return ()
