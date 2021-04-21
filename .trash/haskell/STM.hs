import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Applicative

type Name = String
data Account = Account (TVar (Name, Int))

newAccount :: String -> Int -> STM Account
newAccount name amount = Account <$> newTVar (name, amount)

withdraw :: Account -> Int -> STM ()
withdraw (Account tvar) amount = do
  (name, balance) <- readTVar tvar
  writeTVar tvar (name, balance - amount)

deposit :: Account -> Int -> STM ()
deposit acc amount = withdraw acc (-amount)

transfer :: Account -> Account -> Int -> IO ()
transfer a1 a2 amount = atomically $ do
  withdraw a1 amount
  deposit a2 amount

getBalance :: Account -> STM (Name, Int)
getBalance (Account v) = readTVar v

test :: IO ()
test = do
  son <- atomically $ newAccount "son" 0
  mom <- atomically $ newAccount "mom" 2000
  forkIO $ transfer mom son 1000
  forkIO $ transfer mom son 1000
  bal1 <- atomically $ getBalance mom
  bal2 <- atomically $ getBalance son
  putStrLn $ show bal1
  putStrLn $ show bal2


withdraw1 :: Account -> Int -> STM ()
withdraw1 (Account tvar) amount = do
  (name, balance) <- readTVar tvar
  if (amount > 0) && (amount > balance)
    then retry -- perhaps infinitely
    else writeTVar tvar (name, balance - amount)

test1 :: IO ()
test1 = do
  son <- atomically $ newAccount "son" 0
  atomically $ withdraw1 son 100

test2 :: IO ()
test2 = do
  son <- atomically $ newAccount "son" 0
  draw <- newEmptyMVar
  depo <- newEmptyMVar
  forkIO $ do
    atomically $ withdraw1 son 100
    putMVar draw "Got money!"
  forkIO $ do
    atomically $ deposit son 100
    putMVar depo "Saved money!"
  takeMVar draw
  takeMVar depo
  bal2 <- atomically $ getBalance son
  putStrLn $ show bal2


withdraw2 :: Account -> Int -> STM ()
withdraw2 (Account tvar) amount = do
  (name, balance) <- readTVar tvar
  check (amount > 0 && amount > balance) -- returns bottom when satisfied, retries when not
  writeTVar tvar (name, balance - amount)

transferSTM :: Account -> Account -> Int -> STM ()
transferSTM a1 a2 amount = do
  withdraw2 a1 amount
  deposit a2 amount

transfer2 :: Account -> Account -> Account -> Int -> STM ()
-- transfer2 s m d a = transferSTM m s a `orElse` transferSTM d s a
transfer2 s m d a = transferSTM m s a <|> transferSTM d s a

test3 :: IO ()
test3 = do
  thread1 <- newEmptyMVar
  son <- atomically $ newAccount "son" 0
  mom <- atomically $ newAccount "mom" 500
  dad <- atomically $ newAccount "dad" 2000
  forkIO $ do
    atomically $ transfer2 mom dad son 1000
    putMVar thread1 ()
  takeMVar thread1
  balson <- atomically $ getBalance son
  putStrLn $ show balson
  balmom <- atomically $ getBalance mom
  putStrLn $ show balmom
  baldad <- atomically $ getBalance dad
  putStrLn $ show baldad
