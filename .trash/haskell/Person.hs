-- Person.hs
{-# LANGUAGE DeriveDataTypeable #-}
import Data.Typeable

data Person = Person String Bool deriving (Show, Typeable)

equalTypes :: (Typeable a, Typeable b) => a -> b -> Bool
equalTypes a b = typeOf a == typeOf b
