{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Data.Massiv.Array as Array
import Data.Massiv.Array.IO
import System.Environment
import Criterion.Measurement

printTime :: String -> Double -> IO Double
printTime nm ts = do
  t <- getTime
  putStrLn $ nm <> ": " <> show (t - ts)
  pure t

main :: IO ()
main = do
  initializeTime
  args <- getArgs
  let path = args !! 0
  putStrLn ("open file: " ++ path)
  t0 <- getTime
  -- !image <- readimage path :: io (image s (alpha (srgb 'nonlinear)) word8)
  !image <- readImageAuto path :: IO (Image S (Alpha (SRGB 'NonLinear)) Word8)
  t1 <- printTime "Read" t0
  !image' <-
    computeIO $ Array.map greenify $ image :: IO (Image S (Alpha (SRGB 'NonLinear)) Word8)
  t2 <- printTime "Greenify" t1
  !_ <- writeImage "/tmp/output.png" image'
  _ <- printTime "Write" t2
  pure ()
  where
    greenify pix@(toPixelD -> PixelRGBA r g b a) =
      let r' = r ** 1.5
          g' = g ** 0.8
          b' = b ** 1.5
      in toPixel8 (PixelRGBA r' g' b' a)
