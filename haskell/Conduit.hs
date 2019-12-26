{-# LANGUAGE OverloadedStrings #-}

module Learn where

import Conduit
import Data.Monoid (Sum (..))
import qualified System.IO as IO
import Data.ByteString (ByteString)
import qualified Data.Text as T
import System.FilePath (takeExtension)
import Data.Char (toUpper)
import Crypto.Hash (Digest, SHA256)
import Crypto.Hash.Conduit (sinkHash)
import Network.HTTP.Simple (httpSink)

exec1 = do
  putStrLn "List version:"
  print $ take 10 [1..]
  putStrLn ""
  putStrLn "Conduit version:"
  print $ runConduitPure $ yieldMany [1..] .| takeC 10 .| sinkList

exec2 = do
  putStrLn "List version:"
  print $ takeWhile (< 18) $ map (* 2) $ take 10 [1..]
  putStrLn ""
  putStrLn "Conduit version:"
  print $ runConduitPure
        $ yieldMany [1..]
       .| takeC 10
       .| mapC (*2)
       .| takeWhileC (< 18)
       .| sinkList

exec3 = do
  putStrLn "List version:"
  mapM_ print $ takeWhile (< 18) $ map (* 2) $ take 10 [1..]
  putStrLn ""
  putStrLn "Conduit version:"
  runConduit
        $ yieldMany [1..]
       .| takeC 10
       .| mapC (*2)
       .| takeWhileC (< 18)
       .| mapM_C print

magic :: Int -> IO Int
magic x = do
  putStrLn $ "I'm doing magic with " ++ show x
  return $ x * 2

exec4 = do
  putStrLn "List version:"
  mapM magic (take 10 [1..]) >>= mapM_ print . takeWhile (< 18) -- This is incorrect!
  putStrLn ""
  putStrLn "Conduit version:"
  runConduit $
       yieldMany [1..]
    .| takeC 10
    .| mapMC magic
    .| takeWhileC (< 18)
    .| mapM_C print

exec5 = print $ getSum $ runConduitPure $ yieldMany [1..100] .| foldMapC Sum

exec6 = putStrLn $ runConduitPure
  $ yieldMany [1..10]
  .| mapC show
  .| unlinesC
  .| foldC

exec7 = runConduit
        $ yieldMany (map (replicate 5) [1..10])
        .| concatC
        .| mapM_C print

evenM :: Int -> IO Bool
evenM i = do
  let res = even i
  print (i, res)
  return res

exec8 = runConduit
  $ yieldMany [1..10]
  .| filterMC evenM
  .| mapM_C print

iterMC' :: Monad m => (a -> m ()) -> ConduitT a a m ()
iterMC' inspect = mapMC (inspect >> return)

source :: Monad m => ConduitT i Int m ()
source = do
  yieldMany [1..10]
  yieldMany [11..20]

exec9 = runConduit $ source .| mapM_C print

sink :: Monad m => ConduitT Int o m (String, Int)
sink = do
  x <- takeC 5 .| mapC show .| foldC
  y <- sumC
  return (x,y)

exec10 = do
  let res = runConduitPure $ yieldMany [1..10] .| sink
  print res

trans :: Monad m => ConduitT Int Int m ()
trans = do
  takeC 5 .| mapC (+ 1)
  mapC (* 2)

exec11 = runConduit $ yieldMany [1..10] .| trans .| mapM_C print

yieldMany' :: Monad m => [a] -> ConduitT i a m ()
yieldMany' = mapM_ yield

exec12 :: IO ()
exec12 = do
  print $ runConduitPure $ yield 1 .| await
  -- print $ runConduitPure $ yieldMany [] .| await
  -- print $ runConduitPure $ return () .| await
  print $ runConduitPure await

mapC' :: Monad m => (i -> o) -> ConduitT i o m ()
mapC' f = loop
  where loop = do
          mx <- await
          case mx of
            Nothing -> return ()
            Just x -> do
              yield (f x)
              loop

filterC' :: Monad m => (i -> Bool) -> ConduitT i i m ()
filterC' f = loop
  where loop = do
          mx <- await
          case mx of
            Nothing -> return ()
            Just x -> if f x
                      then do yield x
                              loop
                      else loop

mapMC' :: Monad m => (a -> m b) -> ConduitT a b m ()
mapMC' k = loop
  where loop = do
          mx <- await
          case mx of
            Nothing -> return ()
            Just x -> do
              y <- lift (k x)
              yield y
              loop

takeWhileC' :: Monad m => (a -> Bool) -> ConduitT a a m ()
takeWhileC' pred = loop
  where loop = do
          mx <- await
          case mx of
            Nothing -> return ()
            Just x
              | pred x -> do
                  yield x
                  loop
              | otherwise -> leftover x

exec13 = print $ runConduitPure $ yieldMany [1..10] .| do
  x <- takeWhileC' (<=5) .| sinkList
  y <- sinkList
  return (x,y)

exec14 = runConduit $
     yieldMany [1..10]
  .| iterMC print
  .| liftIO (putStrLn "I was called")
  .| sinkNull

exec15 = IO.withBinaryFile "input.txt" IO.ReadMode $ \inH ->
  IO.withBinaryFile "output.txt" IO.WriteMode $ \outH ->
  runConduit $ sourceHandle inH .| sinkHandle outH

exec16 :: IO ()
exec16 = withSourceFile "input.txt" $ \source ->
  withSinkFile "output.txt" $ \sink ->
  runConduit $ source .| sink

sourceFile' :: MonadResource m => FilePath -> ConduitT i ByteString m ()
sourceFile' fp =
  bracketP (IO.openBinaryFile fp IO.ReadMode) IO.hClose sourceHandle

sinkFile' :: MonadResource m => FilePath -> ConduitT ByteString o m ()
sinkFile' fp =
  bracketP (IO.openBinaryFile fp IO.WriteMode) IO.hClose sinkHandle

exec17 :: IO ()
exec17 = runResourceT
  $ runConduit
  $ sourceFile' "input.txt"
  .| sinkFile' "output.txt"

exec18 :: IO ()
exec18 = runConduitRes
  $ sourceDirectoryDeep True "."
  .| filterC (\fp -> takeExtension fp == ".hs")
  .| awaitForever sourceFile
  .| sinkFile "all-haskell-files"

exec19 :: IO ()
exec19 = runConduitRes
  $ sourceFile "input.txt"
  .| decodeUtf8C
  .| omapCE toUpper
  .| encodeUtf8C
  .| stdoutC

exec20 :: IO ()
exec20 = runConduitRes
  $ sourceFile "input.txt"
  .| decodeUtf8C
  .| takeWhileCE (/= '\n')
  .| encodeUtf8C
  .| stdoutC

doubles :: [Double]
doubles = [1, 2, 3, 4, 5, 6]

average :: Monad m => ConduitT Double Void m Double
average = getZipSink (go <$> ZipSink sumC <*> ZipSink lengthC)
  where go total len = total / fromIntegral len

exec21 = print $ runConduitPure $ yieldMany doubles .| average

exec22 :: IO (Digest SHA256)
exec22 = do
  digest <- runResourceT $ httpSink "http://httpbin.org"
            (\_res -> getZipSink (ZipSink (sinkFile "output.txt") *> ZipSink sinkHash))
  return digest

fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (drop 1 fibs)

indexedFibs :: ConduitT () (Int,Int) IO ()
indexedFibs = getZipSource
  $ (,)
  <$> ZipSource (yieldMany [1..])
  <*> ZipSource (yieldMany fibs)

exec23 = runConduit $ indexedFibs .| takeC 10 .| mapM_C print

tagger :: Monad m => ConduitT Int (Either Int Int) m ()
tagger = mapC $ \i -> if even i then Left i else Right i

evens, odds :: Monad m => ConduitT Int String m ()
evens = mapC $ \i -> "Even number: " ++ show i
odds  = mapC $ \i -> "Odd  number: " ++ show i

left :: Either l r -> Maybe l
left = either Just (const Nothing)

right :: Either l r -> Maybe r
right = either (const Nothing) Just

inside :: Monad m => ConduitT (Either Int Int) String m ()
inside = getZipConduit
  $ ZipConduit (concatMapC left .| evens)
  *> ZipConduit (concatMapC right .| odds)

exec24 = runConduit $ enumFromToC 1 10 .| tagger .| inside .| mapM_C putStrLn

withFiveSum :: Monad m
            => ConduitT Int o m r
            -> ConduitT Int o m (r, Int)
withFiveSum inner = do
  r <- takeExactlyC 5 inner
  -- r <- takeC 5 .| inner
  s <- sumC
  return (r,s)

exec25 = print $ runConduitPure $ yieldMany [1..10] .| withFiveSum (return ())

exec26 :: IO ()
exec26 = runConduitRes $ sourceFile "input.txt" .| decodeUtf8C .| peekForeverE (do
    len <- lineC lengthCE
    liftIO $ print len)
