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

main :: IO ()
main = do
  args <- getArgs
  initFFmpeg
  (nextFrame, cleanup) <- imageReaderTime (File (args!!(0)))
  startTime <- getCurrentTime
  let go = nextFrame >>= \case
        Nothing -> pure ()
        Just (frame, timestamp) -> do
          curTime <- getCurrentTime
          let elapsed = realToFrac $ diffUTCTime curTime startTime
          print elapsed
          when (timestamp > elapsed) $ do
            threadDelay (round ((timestamp - elapsed) * 1_000_000))
          hPutStr stdout $ Vec.toList (textify frame)
          hFlush stdout
          go
  go
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
      blockIsBlack  y x = sumBlock y x < (block_h * block_w * 255) `div` 2
  in Vec.create $ do
    buf <- MVec.new (h * (w+1))
    forM [0..(h-1)] $ \y -> do
      forM [0..(w-1)] $ \x -> do
        MVec.write buf (y*(w+1)+x) (if blockIsBlack y x then '#' else ' ')
      MVec.write buf (y*(w+1)+w) '\n'
    return buf
