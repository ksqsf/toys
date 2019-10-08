{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE DeriveAnyClass #-}
#endif

module Lib where

import Codec.Compression.GZip
import Data.Binary
import qualified Data.ByteString.Lazy as DB
import Numeric (showHex)

data Exp = IntE Int | OpE String Exp Exp deriving (Show)

instance Binary Exp where
  put (IntE i) = do
    put (0 :: Word8)
    put i
  put (OpE s e1 e2) = do
    put (1 :: Word8)
    put s
    put e1
    put e2
  get = do
    t <- get :: Get Word8
    case t of
      0 -> do
        i <- get
        return (IntE i)
      1 -> do
        s <- get
        e1 <- get
        e2 <- get
        return (OpE s e1 e2)

foo = OpE "*" (IntE 5) (IntE 10)

pprint :: DB.ByteString -> String
pprint = concatMap (flip showHex "") . DB.unpack
