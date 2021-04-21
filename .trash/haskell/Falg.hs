{-# LANGUAGE DeriveFunctor #-}
import Data.Functor

-- Definition of F-algebra and F-coalgebra
type Algebra f a = f a -> a
type Coalgebra f a = a -> f a

-- Definition of fixed point µF
newtype Fix f = In { unFix :: f (Fix f) }
out :: Fix f -> f (Fix f)
out = unFix

-- cata f . In = f . fmap (cata f)
-- In 和 unFix 是一对同构态射
cata :: (Functor f) => Algebra f a -> Fix f -> a
cata f = f . fmap (cata f) . unFix

ana :: (Functor f) => Coalgebra f a -> a -> Fix f
ana g = In . fmap (ana g) . g

data ListF a s = NilF | ConsF a s deriving Functor
-- 由函子 ListF a 组成的 F-代数 f_a 是一个匹配了 ListF 两个值构造子的函数
-- 具体定义随 s 变化而不同

data List a = Nil | Cons a (List a) deriving Functor

lenAlg :: ListF a Int -> Int
lenAlg NilF = 0
lenAlg (ConsF a n) = 1 + n

muList :: List a -> Fix (ListF a)
muList Nil = In NilF
muList (Cons a xs) = In (ConsF a (muList xs))

len :: List a -> Int
len = cata lenAlg . muList


-- 
data StreamF a s = StreamF a s deriving (Functor)
type Stream a = Fix (StreamF a)

genCoalg :: Int -> StreamF Int Int
genCoalg a = StreamF a a

genStream :: Int -> Stream Int
genStream = ana genCoalg
