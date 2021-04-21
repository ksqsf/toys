{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}

module TypeArity where

import Data.Proxy
import Language.Haskell.TH

class TypeArity (cla :: k) where
  arity :: Proxy cla -> Integer

getTypeArity :: Name -> Q Int
getTypeArity name = do
  info <- reify name
  case info of
    TyConI dec ->
      case dec of
        DataD _ _ _ tvbs cons _ -> return $ length tvbs
        NewtypeD _ _ _ tvbs con _ -> return $ length tvbs
        _ -> error "The type must be data or newtype"
    _ -> error "bad type name"

makeTypeArity :: Name -> Q [Dec]
makeTypeArity name = do
  at <- getTypeArity name
  [d| instance TypeArity $(conT name) where
        arity _ = at |]

