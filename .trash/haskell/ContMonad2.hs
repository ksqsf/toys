import Control.Monad.Cont

print4 :: ContT r IO ()
print4 = do
  (goto, n) <- callCC $ \k ->
    let f x = k (f, x)
    in return (f, 0)
  if n < 4
    then do liftIO $ putStrLn "Hello"
            goto (n+1)
    else return ()

fact_cps2 :: Int -> Cont r Int
fact_cps2 n = do
  (goto, acc, num) <-
    callCC $ \k ->
      let f x y = k (f, x, y)
      in return (f, 1, n)
  if num == 1
    then return acc
    else goto (acc * num) (num - 1)

-- 展开 callCC
fact_cps2' n =
  (ContT $ \h -> runContT (let f x y = ContT $ \_ -> h (f, x, y) in return (f,1,n)) h)
  >>=
  (\(goto, acc, num) ->
     if num == 1
     then return acc
     else goto (acc * num) (num - 1))

-- 展开 >>=
fact_cps2'' n =
  ContT $ \br ->
            runContT
            (ContT $ \h -> runContT
                           (let f x y = ContT $ \_ -> h (f, x, y)
                             in return (f,1,n)) h)
            (\(goto,acc,num) -> runContT
              (if num == 1
                then return acc
                else goto (acc * num) (num - 1))
              br)

-- 去掉 Cont
fact_cps2''' n br = (\h -> (let f x y = \_ -> h (f, x, y)
                            in \k -> k (f,1,n)) h)
                    (\(goto,acc,num) -> (if num == 1 
                                         then (\k -> k acc)
                                         else goto (acc*num) (num-1))
                                        br)

-- 替换 h
fact_cps2'''' n br = (let f x y = \_ -> (\(goto,acc,num) -> (if num == 1 
                                                             then (\k -> k acc)
                                                             else goto (acc*num) (num-1))
                                          br) (f, x, y)
                      in \k -> k (f, 1, n))
                     (\(goto,acc,num) -> (if num == 1 
                                          then (\k -> k acc)
                                          else goto (acc*num) (num-1))
                                         br)

-- infinite loop
loop :: Cont r Int
loop = do
  (goto, acc) <-
    callCC $ \k ->
      let f x = k (f, x)
      in return (f, 1)
  goto acc

loop' br = (\h -> (let f x = \_ -> h (f, x) in \k -> k (f,())) h)
           (\(goto,()) -> (goto ()) br)

loop'' br = (\h -> (let f = \_ -> h f in \k -> k f) h)
            (\goto -> goto br)

magic h = (let f x = \_ -> h (f, x) in \k -> k (f,())) h

-- fix :: (a -> a) -> a
-- magic' :: ((p -> t) -> t) -> t
magic' h = (let f _ = h f in \k -> k f) h

-- magic'' :: (t -> t) -> t
magic'' f = let x = f x in f x
