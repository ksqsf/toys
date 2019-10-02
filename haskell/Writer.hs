import Control.Monad
import Control.Monad.Writer

fib :: Int -> Writer String Int
fib 0 = do
  tell "fib 0 = 0\n"
  return 0
fib 1 = do
  tell "fib 1 = 1\n"
  return 1
fib n = do
  n1 <- fib (n-1)
  n2 <- fib (n-2)
  let n = n1 + n2
  tell ("fib " ++ show n ++ " = " ++ show n1 ++ " + " ++ show n2 ++ " = " ++ show n ++ "\n")
  return n
