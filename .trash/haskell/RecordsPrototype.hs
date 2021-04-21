{-# LANGUAGE KindSignatures, DataKinds, MultiParamTypeClasses, TypeFamilies, RankNTypes, FlexibleInstances, UndecidableInstances, PolyKinds, FlexibleContexts, NoMonomorphismRestriction, TypeOperators, GADTs #-}

import Data.Typeable
import GHC.Types

-- type Symbol = String

-- FlyTy String "name"
-- 可以解析到某种类型，即 field 的 type
type family FldTy (t :: *) (n :: Symbol) :: *

-- UpdTy String "name" ???
type family UpdTy (t :: *) (n :: Symbol) (a :: *) :: *

-- Has Person "name"
-- Has Company "name"
class Has r n where
  getField :: Proxy n -> r -> FldTy r n

class (Has r n, r ~ UpdTy r n (FldTy r n)) => Upd r n a where
  setField :: Proxy n -> r -> a -> UpdTy r n a

-- 合并 Has 和 Upd
-- p 是类型构造器，r 是数据类型，n 是访问器名字
class Accessor (p :: * -> * -> *) (r :: *) (n :: Symbol) where
  accessField :: Proxy n -> -- 传进来类型 n （实际上是访问器的名字）
              (Has r n => r -> FldTy r n) -> -- 访问器；所有有 n 的类型有访问器 r -> FieldType
              (forall a. Upd r n a => r -> a -> UpdTy r n a) -> -- 更新器
              p r (FldTy r n) -- 构造出某种类型

instance Has r n => Accessor (->) r n where
  accessField _ g _ = g

field :: Accessor p r n => Proxy n -> p r (FldTy r n)
field z = accessField z (getField z) (setField z)

foo :: Accessor p r "foo" => p r (FldTy r "foo")
foo = field (Proxy :: Proxy "foo")

data R a = MkR { foo1 :: a -> a }

type instance FldTy (R a) "foo" = a -> a
type instance UpdTy (R a) "foo" (b -> b) = R b

instance Has (R a) "foo" where
  getField _ r = foo1 r

instance (t ~ (b -> b)) => Upd (R a) "foo" t where
  setField _ (MkR _) x = MkR x -- 替换参数为 x

data U a = MkU { foo2 :: R a, bar1 :: a }
type instance FldTy (U a) "foo" = R a
type instance UpdTy (U a) "foo" (R c) = U c

instance Has (U a) "foo" where
  getField _ = foo2

instance t ~ R a => Upd (U a) "foo" t where
  setField _ (MkU _ y) x = MkU x y

overloadTestR :: R Int
overloadTestR = MkR (+1)

overloadTestU :: U Int
overloadTestU = MkU overloadTestR 100

