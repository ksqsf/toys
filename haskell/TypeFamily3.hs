{-# LANGUAGE TypeFamilies #-}
import Data.Vector
import Data.Sequence

-- | data family

-- data family Array a
-- data instance Array Int = MkArrayInt (Vector Int)
-- data instance Array Char = MkArrayInt (Seq Char)

-- | type

type family Array a :: *
type instance Array Int = Vector Int
type instance Array Char = Seq Char

