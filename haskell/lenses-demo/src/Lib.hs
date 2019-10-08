{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
module Lib where

import Control.Lens

data Name = N
  { _firstName :: String
  , _familyName :: String
  } deriving (Show, Eq)

makeLenses ''Name

name = N "Song" "Zhang"


