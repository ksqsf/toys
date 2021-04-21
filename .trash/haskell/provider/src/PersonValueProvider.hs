{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass, DeriveDataTypeable #-}
module PersonValueProvider where
import Data.Aeson
import GHC.Generics
import Data.String
import Data.Maybe
import Data.Data
import Language.Haskell.TH.Quote
import Language.Haskell.TH

data Person = Person {name::String, age::Int}
  deriving (Show, Generic, FromJSON, Data)

song = Person "Song ZHANG" 26

quoteJSONPerson :: String -> ExpQ
quoteJSONPerson p = dataToExpQ (const Nothing)
                    ((fromJust.decode.fromString) p :: Person)

personJSON :: QuasiQuoter
personJSON = QuasiQuoter {quotePat = undefined, quoteType = undefined, quoteDec = undefined,  quoteExp = quoteJSONPerson }

personJSON_file = quoteFile personJSON
