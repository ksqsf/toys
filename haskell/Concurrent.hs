import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import System.Exit (exitSuccess)

-- type Account = IORef Integer

-- transfer :: Integer -> Account -> Account -> IO ()
-- transfer amount from to = do
--   fromVal <- readIORef from
--   toVal   <- readIORef to
--   writeIORef from (fromVal - amount)
--   writeIORef to (toVal + amount)

type Account = TVar Integer

credit :: Integer -> Account -> STM ()
credit amount account = do
  current <- readTVar account
  writeTVar account (current + amount)

debit :: Integer -> Account -> STM ()
debit amount account = do
  current <- readTVar account
  writeTVar account (current - amount)

transfer :: Integer -> Account -> Account -> STM ()
transfer amount from to = do
  fromVal <- readTVar from
  if fromVal - amount >= 0
    then do debit amount from
            credit amount to
    else retry

newAccount :: Integer -> IO Account
newAccount amount = newTVarIO amount

main = do
  bob <- newAccount 10000
  jill <- newAccount 4000
  replicateM_ 2000 $ forkIO $ atomically $ transfer 1 bob jill
  forever $ do
    bobBalance <- atomically $ readTVar bob
    jillBalance <- atomically $ readTVar jill
    putStrLn ("Bob's balance: " ++ show bobBalance ++ ", Jill's balance: " ++ show jillBalance)
    if bobBalance == 8000
      then exitSuccess
      else putStrLn "Trying again."

forkJoinIO :: [IO a] -> IO [a]
forkJoinIO jobs = do
  let n = length jobs
  mvs <- replicateM n newEmptyMVar
  tids <- traverse (\(job, mv) -> forkIO $ op job mv) (zip jobs mvs)
  answers <- traverse takeMVar mvs
  return answers
  where
    op job mv = do
      ans <- job
      putMVar mv ans

forkJoinIO_ :: [IO a] -> IO ()
forkJoinIO_ jobs = do
  let n = length jobs
  mvs <- replicateM n newEmptyMVar
  tids <- traverse (\(job, mv) -> forkIO $ op job mv) (zip jobs mvs)
  _ <- traverse takeMVar mvs
  return ()
  where
    op job mv = do
      _ <- job
      putMVar mv ()
