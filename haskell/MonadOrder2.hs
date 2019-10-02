{-# LANGUAGE DeriveFunctor #-}
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer

pushWS :: Int -> WriterT String (State [Int]) ()
pushWS x = WriterT $ state $ \xs -> (((), " push " ++ show x), x:xs)

popWS :: WriterT String (State [Int]) Int
popWS = WriterT $ state $ \(x:xs) -> ((x, " pop " ++ show x), xs)

-- 这种组合方式：Writer 更靠近 Int，记录和结果一起返回（在State看来）

push :: Int -> StateT [Int] (Writer String) ()
push x = StateT $ \xs -> writer (((), x:xs), " push " ++ show x)

pop :: StateT [Int] (Writer String) Int
pop = StateT $ \(x:xs) -> writer ((x, xs), " pop " ++ show x)

-- 记录在外面，状态和结果一起返回（在Writer看来）
-- 这两种写法没有任何区别!!
-- 与下面的写法是等价的

newtype WS s w a = WS { runWS :: s -> (a,s,w) } deriving (Functor)

instance Monoid w => Monad (WS s w) where
  return a = WS $ \s -> (a, s, mempty)
  m >>= k = WS $ \s ->
    let (a,s',w) = runWS m s in
    let (b,s'',w') = runWS (k a) s' in
    (b, s'', w `mappend` w')

instance Monoid w => Applicative (WS s w) where
  pure = return
  (<*>) = ap


