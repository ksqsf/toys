#!/usr/bin/env stack
{- stack --resolver lts-14.20 script
   --package safe-exceptions
-}
{-# OPTIONS_GHC -Wall -Werror #-}
module Main where
import Control.Exception.Safe (Exception, MonadThrow, SomeException, throwM)
import Data.Typeable (TypeRep, Typeable, typeRep)
import Text.Read (readMaybe)

data ReadException = ReadException String TypeRep
  deriving (Typeable)

instance Show ReadException where
  show (ReadException s typ) = concat
    [ "Unable to parse as "
    , show typ
    , ": "
    , show s
    ]


instance Exception ReadException

readM :: (MonadThrow m, Read a, Typeable a) => String -> m a
readM s = res
  where res =
          case readMaybe s of
            Just x -> return x
            Nothing -> throwM $ ReadException s (typeRep res)

main :: IO ()
main = do
  print (readM "hello" :: Either SomeException Int)
  print (readM "5" :: Either SomeException Int)
  print (readM "5" :: Either SomeException Bool)

  -- Also works in plain IO
  res1 <- readM "6"
  print (res1 :: Int)
  res2 <- readM "not an int"
  print (res2 :: Int) -- will never get called
