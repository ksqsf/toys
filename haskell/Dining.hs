import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import System.IO
import System.Random

type Name = String

philosopherNames :: [Name]
philosopherNames = ["Russell", "Plato", "Hobbes", "Locke", "Aristotle"]

getName :: Int -> String
getName = (philosopherNames !!)

type Fork = TVar Bool

newFork :: STM Fork
newFork = newTVar True

takeFork, putFork :: Fork -> STM ()
takeFork fork = do
  isFree <- readTVar fork
  check isFree
  writeTVar fork False
putFork fork = writeTVar fork True

randomDelay :: IO ()
randomDelay = do
  time <- getStdRandom (randomR (1,3))
  threadDelay (time * 10^6)

type Philosopher = (Name, Fork, Fork)

philosopher :: Philosopher -> IO ()
philosopher (name, fork1, fork2) = do
  putStrLn (name ++ " is thinking.")
  randomDelay
  atomically $ do
    takeFork fork1
    takeFork fork2
  putStrLn (name ++ " is eating.")
  atomically $ do
    putFork fork1
    putFork fork2
  philosopher (name, fork1, fork2)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  tvar <- newEmptyMVar
  forks <- replicateM 5 (atomically newFork)
  forM_ [0..4] $ \i -> forkIO
    (philosopher (getName i, forks!!i, forks!!((i+1) `mod` 5)))
  takeMVar tvar
