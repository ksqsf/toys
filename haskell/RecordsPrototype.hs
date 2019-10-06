{-# LANGUAGE KindSignatures, DataKinds, MultiParamTypeClasses, TypeFamilies, RankNTypes, FlexibleInstances, UndecidableInstances, PolyKinds, FlexibleContexts, NoMonomorphismRestriction, TypeOperators, GADTs #-}

import Data.Typeable


type Symbol = String

-- FlyTy String "name"
type family FldTy (t :: *) (n :: Symbol) :: *

-- UpdTy String "name" ???
type family UpdTy (t :: *) (n :: Symbol) (a :: *) :: *

-- Has String "name"
class Has r n where
  getField :: Proxy n -> r -> FldTy r n

-- Upd String "name" ???
class (Has r n, r ~ UpdTy r n (FldTy r n)) => Upd r n a where
  setField :: Proxy n -> r -> a -> UpdTy r n a

class Accessor (p :: * -> * -> *) (r :: *) (n :: Symbol) where
  accessField :: Proxy n ->
              (Has r n => r -> FldTy r n) ->
              (forall a. Upd r n a => r -> a -> UpdTy r n a) ->
              p r (FldTy r n)

instance Has r n => Accessor (->) r n where
  accessField _ g _ = g

