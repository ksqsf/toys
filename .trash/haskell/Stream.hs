import System.IO


bracket :: IO a
        -> (a -> IO b)
        -> (a -> IO c)
        -> IO c
bracket acquire release action = do
  h <- acquire
  result <- action h
  release h
  return result


