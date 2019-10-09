{-# LANGUAGE CPP, DeriveGeneric, DeriveDataTypeable #-}

module Macro1 where

import GHC.Generics
import Data.Typeable
import Data.Data
import Data.Aeson
import Text.PrettyPrint.GenericPretty

#define DERIVING deriving (Show, Generic, Data, Typeable)
data Company = C {departments :: [Department]} DERIVING
data Department = D {departmentName :: String,
                     manager :: Person,
                     workers :: [Person] } DERIVING
data Person = P {personName :: Name,
                 gender :: Gender,
                 age    :: Age } DERIVING
data Name = N {familyName :: String,
               givenName  :: String } DERIVING
data Gender = Male | Female DERIVING
type Age = Int

#define MAX(a,b) (if ((a) < (b)) \
                     then (b) \
                     else (a))

foo = MAX(10,20)

