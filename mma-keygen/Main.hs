{-# LANGUAGE Strict #-}
module Main where

import           Data.Function ((&))
import           Control.Monad (when)
import           Data.Bits
import           Data.Bool (bool)
import qualified Data.ByteString.Builder as BS
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Char (ord)
import           Data.List (foldl')
import           Data.Word (Word8)
import           System.IO (hFlush, stdout)
import           System.Random (randomRIO)

-- Mathematica 12, 13 magic numbers
magicNumbers :: [Int]
magicNumbers = [10690, 12251, 17649, 24816, 33360, 35944, 36412, 42041, 42635, 44011, 53799, 56181, 58536, 59222, 61041]

-- System Modeler 12/13 magic numbers
-- [4912, 4961, 22384, 24968, 30046, 31889, 42446, 43787, 48967, 61182, 62774]

mathIdValid :: BS.ByteString -> Bool
mathIdValid mathID = BS.length mathID == 16 && all charOK (zip [0..] (BS.unpack mathID))
  where
    charOK :: (Int, Char) -> Bool
    charOK (4, c) = c == '-'
    charOK (10, c) = c == '-'
    charOK (_, c) = BS.elem c "0123456789"

genActivationKey :: IO ByteString
genActivationKey = BS.pack <$> sequenceA [r, r, r, r, pure '-', r, r, r, r, pure '-', r, r, r, r, r, r]
  where r = randomRIO ('0', '9')

f1 :: Int -> Int -> Int -> Int
f1 n byte c = foldl' f n [0..7]
  where
    f n i = let bit = (byte `shiftR` i) .&. 1 in
      if bit + ((n - bit) .&. (complement 1)) == n
      then (n - bit) `shiftR` 1
      else ((c - bit) `xor` n) `shiftR` 1

genPassword :: ByteString -> ByteString -> Int -> ByteString
genPassword mathID activationKey init =
  let str = mathID <> "$1&" <> activationKey
      hash = BS.foldl' (\hash c -> f1 hash (ord c) 0x105c3) init (BS.reverse str)
      n1 = iterateN hash 0x105c3 0
        & \n1 -> floor $ realToFrac ((n1 + 0x72fa) .&. 0xffff) * 99999.0 / 0xffff
      n1s = stringify n1
      temp = (read . BS.unpack $ n1s#(0,-3) <> takeLast 2 n1s <> n1s#(-3,-2))
        & \temp -> (ceiling $ (realToFrac temp / 99999.0) * 0xffff)
        & \temp -> f1 (f1 0 (temp .&. 0xff) 0x1064b) (temp `shiftR` 8) 0x1064b
        & \temp -> BS.foldl' (\t c -> f1 t (ord c) 0x1064b) temp (BS.reverse str)
      n2 = iterateN temp 0x1064b 0
        & \n2 -> floor $ realToFrac (n2 .&. 0xffff) * 99999.0 / 0xffff
      n2s = stringify n2
  in LBS.toStrict . BS.toLazyByteString $
       n2s!3 <> n1s!3 <> n1s!1 <> n1s!0 <> BS.char8 '-'
    <> n2s!4 <> n1s!2 <> n2s!0 <> BS.char8 '-'
    <> n2s!2 <> n1s!4 <> n2s!1 <> BS.byteString "::1"
  where
    bs ! i = BS.char8 (bs `BS.index` i)
    bs # (x, y) = BS.take (j-i) . BS.drop i $ bs
      where n = BS.length bs
            (i, j) = (x `mod` n, y `mod` n)
    stringify x = takeLast 5 $ "0000" <> BS.pack (show x)

    takeLast :: Int -> ByteString -> ByteString
    takeLast n bs = BS.drop (BS.length bs - n) bs

    iterateN :: Int -> Int -> Int -> Int
    iterateN p q n =
      if f1 (f1 p (n .&. 0xff) q) (n `shiftR` 8) q /= 0xa5b6
      then if n + 1 >= 0xffff
           then error "n compute error"
           else iterateN p q (n+1)
      else n

main = do
  BS.putStr "MathID: "
  hFlush stdout
  mathID <- BS.getLine
  when (not (mathIdValid mathID)) $
    error "Invalid Math ID!"
  activationKey <- genActivationKey
  magic <- randomChoice magicNumbers
  let password = genPassword mathID activationKey magic

  BS.putStr "Activation Key: "
  BS.putStrLn activationKey
  BS.putStr "      Password: "
  BS.putStrLn password

  where
    randomChoice :: [a] -> IO a
    randomChoice xs = do
      i <- randomRIO (0, length xs - 1)
      pure (xs !! i)
