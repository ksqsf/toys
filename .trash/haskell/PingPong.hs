{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

import GHC.Generics
import Data.Binary
import Data.Typeable

data Ping = Ping ProcessId
  deriving (Generic, Typeable, Binary)
data Pong = Pong ProcessId
  deriving (Generic, Typeable, Binary)

pingPong :: Process ()
pingPong = forever $ do
  self <- getSelfPid
  Ping pid <- expect
  say $ "Ping from " ++ show pid
  send pid (Pong self)
