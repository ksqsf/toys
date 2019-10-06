{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
import Data.Proxy

-- type role phantom
-- data Proxy (a :: k) = Proxy


id' :: Proxy t -> t -> t
id' p t = asProxyTypeOf t p


type TypeRep = String

class Typeable (t :: k) where
  typeOf :: Proxy t -> TypeRep

instance (Typeable f, Typeable a) => Typeable (f a) where
  typeOf _ = typeOf (Proxy :: Proxy f) ++ typeOf (Proxy :: Proxy a)
