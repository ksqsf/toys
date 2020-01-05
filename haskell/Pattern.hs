#!/usr/bin/env stack
-- stack --resolver lts-14.20 script --package async --package mtl --package stm --package lifted-async
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Concurrent.Async.Lifted.Safe
import Control.Monad.Reader
import Control.Concurrent.STM
import qualified Control.Monad.State.Strict as State

data Env = Env
  { envLog :: !(String -> IO ())
  , envBalance :: !(TVar Int)
  }

class HasLog a where
  getLog :: a -> (String -> IO ())
instance HasLog (String -> IO ()) where
  getLog = id
instance HasLog Env where
  getLog = envLog

class HasBalance a where
  getBalance :: a -> TVar Int
instance HasBalance (TVar Int) where
  getBalance = id
instance HasBalance Env where
  getBalance = envBalance

class Monad m => MonadBalance m where
  modifyBalance :: (Int -> Int) -> m ()
instance (HasBalance env, MonadIO m) => MonadBalance (ReaderT env m) where
  modifyBalance f = do
    env <- ask
    liftIO $ atomically $ modifyTVar' (getBalance env) f
instance Monad m => MonadBalance (State.StateT Int m) where
  modifyBalance = State.modify

modify :: MonadBalance m
       => (Int -> Int)
       -> m ()
modify f = modifyBalance f

logSomething :: (MonadReader env m, HasLog env, MonadIO m)
             => String
             -> m ()
logSomething msg = do
  env <- ask
  liftIO $ envLog env msg

main :: IO ()
main = do
  ref <- newTVarIO 4
  let env = Env
        { envLog = putStrLn
        , envBalance = ref
        }
  runReaderT
    (concurrently
     (modify (+1))
     (logSomething "Increasing account balance"))
    env
  balance <- readTVarIO ref
  putStrLn $ "Final balance: " ++ show balance
