{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
module Lib where

import Control.Lens

data Name = N
  { _firstName :: String
  , _familyName :: String
  , _hasCircle :: Maybe Circle
  } deriving (Show, Eq)

data Circle = Circle
  { _members :: [Name] }
  deriving (Show, Eq)

$(makeLenses ''Name)
$(makeLenses ''Circle)

jack = N "Jack" "Ma" Nothing
mike = N "Mike" "KKK" Nothing
circle = Circle [jack, mike]
zs = N "Song" "Zhang" (Just circle)

friends = toListOf (hasCircle.traverse.members.traverse)
