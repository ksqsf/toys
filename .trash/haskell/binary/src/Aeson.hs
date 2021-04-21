{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module MyAeson where

import GHC.Generics
import Data.Aeson
import Data.String
import Control.Applicative

ind :: IsString a => a
ind = "{ \"name\": \"Flour\", \"quantity\": 250, \"measure\": \"gr\" }"

type Measure = String

data Ingredient = Ingredient
  { name :: String
  , quantity :: Int
  , measure :: Maybe Measure
  }
  deriving (Show, Eq, Generic)

instance FromJSON Ingredient
instance ToJSON Ingredient


