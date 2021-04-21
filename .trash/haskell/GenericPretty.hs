{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}
import Data.Tree
import GHC.Generics
import Text.PrettyPrint.GenericPretty

deriving instance (Tree a)

instance (Out a) => Out (Tree a)

directory :: Tree String
directory = Node "Home"
  [ Node "Picture" [Node "travel" [], Node "family" []]
  , Node "Video" [Node "Fast and Furious" [], Node "True Lies" []]
  , Node "Music" [Node "My love" [], Node "Destiny" []]
  ]

