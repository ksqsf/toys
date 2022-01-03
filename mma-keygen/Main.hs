{-# LANGUAGE Strict #-}
module Main where

import           Data.Function ((&))
import           Control.Monad (when)
import           Data.Bits
import qualified Data.ByteString.Builder as Builder
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Char (ord)
import           Data.List (foldl')
import           System.IO (hFlush, stdout)
import           System.Random (randomRIO)

-- Mathematica 12, 13 magic numbers
magicNumbers :: [Int]
magicNumbers = [10690, 12251, 17649, 24816, 33360, 35944, 36412, 42041, 42635, 44011, 53799, 56181, 58536, 59222, 61041]

-- System Modeler 12/13 magic numbers
-- [4912, 4961, 22384, 24968, 30046, 31889, 42446, 43787, 48967, 61182, 62774]

mathIdValid :: ByteString -> Bool
mathIdValid mathID = BS.length mathID == 16 && all charOK (zip [0..] (BS.unpack mathID))
  where
    charOK :: (Int, Char) -> Bool
    charOK (4, c) = c == '-'
    charOK (10, c) = c == '-'
    charOK (_, c) = BS.elem c "0123456789"

genActivationKey :: IO ByteString
genActivationKey = BS.pack <$> sequenceA [r, r, r, r, pure '-', r, r, r, r, pure '-', r, r, r, r, r, r]
  where r = randomRIO ('0', '9')

genPassword :: ByteString -> ByteString -> Int -> ByteString
genPassword mathID activationKey magic =
  let str = mathID <> "$1&" <> activationKey
      hash = BS.foldl' (\hash c -> f hash (ord c) 0x105c3) magic (BS.reverse str)
      n1s = iterateN hash 0x105c3 0 & (+ 0x72fa) & normalize & stringify
      seed = (read . BS.unpack $ n1s#(0,-3) <> takeLast 2 n1s <> n1s#(-3,-2))
        & \seed -> (ceiling $ (realToFrac seed / 99999.0) * 0xffff)
        & \seed -> f (f 0 (seed .&. 0xff) 0x1064b) (seed `shiftR` 8) 0x1064b
        & \seed -> BS.foldl' (\seed c -> f seed (ord c) 0x1064b) seed (BS.reverse str)
      n2s = iterateN seed 0x1064b 0 & normalize & stringify
  in LBS.toStrict . Builder.toLazyByteString $
       n2s!3 <> n1s!3 <> n1s!1 <> n1s!0 <> Builder.char8 '-'
    <> n2s!4 <> n1s!2 <> n2s!0 <> Builder.char8 '-'
    <> n2s!2 <> n1s!4 <> n2s!1 <> Builder.byteString "::1"
  where
    bs ! i = Builder.char8 (bs `BS.index` i)       -- Builder from bs[i]
    bs # (x, y) = BS.take (j-i) . BS.drop i $ bs   -- Slice bs[x,y]
      where n = BS.length bs
            (i, j) = (x `mod` n, y `mod` n)
    stringify x = takeLast 5 $ "0000" <> BS.pack (show x)
    normalize n = floor $ realToFrac (n .&. 0xffff) * 99999.0 / 0xffff

    takeLast :: Int -> ByteString -> ByteString
    takeLast n bs = BS.drop (BS.length bs - n) bs

    iterateN :: Int -> Int -> Int -> Int
    iterateN p q n =
      if n >= 0xffff
      then error "n compute error"
      else if f (f p (n .&. 0xff) q) (n `shiftR` 8) q /= 0xa5b6
           then iterateN p q (n+1)
           else n

    f :: Int -> Int -> Int -> Int    -- The 'f1' function in reference.js
    f n byte c = foldl' g n [0..7]
      where
        g n i = let bit = (byte `shiftR` i) .&. 1 in
          if bit + ((n - bit) .&. (complement 1)) == n
          then (n - bit) `shiftR` 1
          else ((c - bit) `xor` n) `shiftR` 1

main = do
  BS.putStr "MathID: "
  hFlush stdout
  mathID <- BS.getLine
  when (not (mathIdValid mathID)) $
    error "Invalid Math ID!"
  activationKey <- genActivationKey
  magic <- choose magicNumbers
  let password = genPassword mathID activationKey magic

  BS.putStr "Activation Key: "
  BS.putStrLn activationKey
  BS.putStr "      Password: "
  BS.putStrLn password

  where
    choose :: [a] -> IO a
    choose xs = (xs !!) <$> randomRIO (0, length xs - 1)
