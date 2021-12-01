module Main where

import           Codec.FFmpeg
import           Codec.Picture
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.ST
import           Data.Time.Clock
import           Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vec
import qualified Data.Vector.Storable.Mutable as MVec
import           System.Environment
import           System.IO
import           Text.Printf

{-
Download the bad apple video: https://archive.org/details/TouhouBadApple
but you can actually use any video file you like. the result won't be good though.

NOTE: -O2 will considerably make the animation smoother!!
-}

main :: IO ()
main = do
  args <- getArgs
  initFFmpeg
  (nextFrame, cleanup) <- imageReaderTime (File (args!!(0)))
  startTime <- getCurrentTime
  hSetBuffering stdout (BlockBuffering (Just 1024))
  let go i = nextFrame >>= \case
        Nothing -> pure ()
        Just (frame, timestamp) -> do
          printf "Frame %s at %.3f second\n" (show i) timestamp
          curTime <- getCurrentTime
          let elapsed = realToFrac $ diffUTCTime curTime startTime
          when (timestamp > elapsed) $ do
            threadDelay (round ((timestamp - elapsed) * 1_000_000))
          hPutStr stdout $ Vec.toList (textify frame)
          hFlush stdout
          go (i+1)
  go 1
  cleanup

textify :: Image Pixel8 -> Vector Char
textify frame =
  let w = 64
      h = 24
      img_w = imageWidth frame
      img_h = imageHeight frame
      img_buf = imageData frame
      block_w = img_w `div` w
      block_h = img_h `div` h
      sumBlock y x = let idx dy = (y*block_h + dy) * img_w + x*block_w
                     in sum [ Vec.sum . Vec.map fromIntegral $ Vec.slice (idx i) block_w img_buf | i <- [0..block_h-1] ]
      blockChar y x =
        let sum = sumBlock y x
            full = block_h * block_w * 255
        in if | sum < full `div` 3     -> '#'
              | sum < full `div` 3 * 2 -> '*'
              | otherwise              -> ' '
  in Vec.create $ do
    buf <- MVec.new (h * (w+1))
    forM [0..(h-1)] $ \y -> do
      forM [0..(w-1)] $ \x -> do
        MVec.write buf (y*(w+1)+x) (blockChar y x)
      MVec.write buf (y*(w+1)+w) '\n'
    return buf
