{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}

newtype Compose f g a = Compose { getCompose :: f (g a) }
newtype (:.:) f g p = Comp1 { unComp1 :: f (g p) }
