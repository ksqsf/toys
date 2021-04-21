{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -ddump-splices #-}
import PersonValueProvider
import Language.Haskell.TH.Quote

song_ZHANG = [personJSON|{"name":"song ZHANG","age":26}|]
nuo_LI = [personJSON_file|src/person.json|]
