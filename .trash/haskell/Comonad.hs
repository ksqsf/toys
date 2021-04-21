-- Comonad == Reverse the arrows of the Kleisli category

{-# LANGUAGE DeriveFunctor #-}

class Functor w => Comonad w where
  extract :: w a -> a -- co-return
  (=>=) :: (w a -> b) -> (w b -> c) -> (w a -> c)
  duplicate :: w a -> w (w a) -- co-join
  extend :: (w a -> b) -> w a -> w b -- co-bind, 从 co-Kleisli composition operator 看到，要得到 wa->c 需要一个 wb，所以要有一个办法取得 wb

  f =>= g = g . extend f
  duplicate = id =>= id
  extend f = (fmap f) . duplicate

  {-# MINIMAL extract, ((=>=)|duplicate|extend) #-}

data Prod a e = Prod a e deriving (Functor, Show)

instance Comonad (Prod s) where
  extract (Prod s e) = e
  wab =>= wbc = \wa@(Prod s a) -> let b = wab wa
                                      c = wbc (Prod s b)
                                  in c
  -- duplicate (Prod s a) = Prod s (Prod s a)
  -- extend wab wa@(Prod s a) = Prod s (wab wa)

data Stream a = Cons a (Stream a) deriving (Functor, Show, Eq, Ord)

instance Comonad Stream where
  extract (Cons a str) = a
  duplicate (Cons a str) = Cons (Cons a str) (duplicate str)

ones :: Num a => Stream a
ones = Cons 1 ones

-- co-Kleisli arrow for a stream is a digital filter
-- extend produces a filtered stream

sumS :: Num a => Int -> Stream a -> a
sumS n (Cons a as) = if n <= 0 then 0 else a + sumS (n-1) as

average :: Fractional a => Int -> Stream a -> a
average n stm = (sumS n stm) / (fromIntegral n)

movingAvg :: Fractional a => Int -> Stream a -> Stream a
movingAvg n = extend (average n)

data Store s a = Store (s->a) s deriving (Functor)

instance Comonad (Store s) where
  extract (Store f a) = f a
  duplicate (Store f a) = Store (Store f) a

-- Store s a encapsulates the idea of focusing on a particular
-- substructure of the data type a using the type s as an index!
--
-- a -> Store s a ===== set + get
