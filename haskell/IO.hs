import Control.Monad
import System.Environment

import System.IO
import System.Process

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> print "?!"
    arg -> mapM_ print args

command1 = do
  (Nothing,Nothing,Nothing,d) <- createProcess (proc "ls" [])
  return ()


