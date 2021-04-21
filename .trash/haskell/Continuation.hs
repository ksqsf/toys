import Control.Monad.Cont

-- fact_cps :: (Eq a, Num a) => a -> (a -> t) -> t
-- fact_cps 0 k = k 1
-- fact_cps n k = fact_cps (n-1) (\x -> k (n*x))

fib_cps :: Int -> ContT r IO Int
fib_cps 0 = return 1
fib_cps 1 = return 1
fib_cps n = do
  n2 <- fib_cps (n-2)
  liftIO $ putStrLn $ "fib_cps " ++ show (n-2) ++ "=" ++ show n2
  n1 <- fib_cps (n-1)
  liftIO $ putStrLn $ "fib_cps " ++ show (n-1) ++ "=" ++ show n1
  return (n1+n2)

fact_cps :: Int -> ContT r IO Int
fact_cps 0 = return 1
fact_cps n = do
  n1 <- fact_cps (n-1)
  callCC $ \k ->
    let r = n * n1
    in if r > 10000
       then k 0
       else return r

data MyCont r a = Cont { runMyCont :: (a -> MyCont r a) -> (a -> MyCont r a) }



print4 :: ContT r IO ()
print4 = do
  (goto, n) <-
    callCC $ \k ->
               let f x = k (f, x)
               in return (f, 0)
  if n < 4
    then do lift $ putStrLn "Hello"
            goto (n+1)
    else return ()

fact_cps2 :: Int -> Cont r Int
fact_cps2 n = do
  (goto, acc, num) <-
    callCC $ \k -> let f x y = k (f, x, y)
                   in return (f, 1, n)
  if num == 1
    then return acc
    else goto (acc * num) (num - 1)
