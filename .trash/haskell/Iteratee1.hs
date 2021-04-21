{-# LANGUAGE DeriveFunctor #-}
import Data.Function (fix)
import Control.Monad
import qualified Control.Exception as Exc
import Control.Monad.IO.Class
import Control.Monad.Trans

data Stream a = Chunks [a] | EOF deriving (Show, Eq, Functor)

instance Monoid (Stream a) where
  mempty = Chunks mempty
  mappend (Chunks xs) (Chunks ys) = Chunks (xs ++ ys)
  mappend _ _ = EOF

instance Semigroup (Stream a) where
  (<>) = mappend

instance Monad Stream where
  return = Chunks . return
  -- mconcat :: Monoid a => [a] -> a
  -- f :: a -> Stream b
  -- fmap f :: [a] -> [Stream b]
  -- mconcat (fmap f xs) :: Stream b
  Chunks xs >>= f = mconcat (fmap f xs)
  EOF >>= _ = EOF

instance Applicative Stream where
  pure = return
  (<*>) = ap

-- a 是流里基本数据的类型（如 Word8）
-- b 是处理结果类型
-- m 是包装 monad
data Step a m b = Continue (Stream a -> Iteratee a m b) -- 接着处理流，因为输入不够
                | Yield b (Stream a) -- 输出 b，还有剩下的输入
                | Error Exc.SomeException -- 出错
                deriving (Functor)

newtype Iteratee s m a = Iteratee { runIteratee :: m (Step s m a) } deriving (Functor)
-- 本质上就是个 Step 的包装


-- smart constructors
returnI :: Monad m => Step a m b -> Iteratee a m b
returnI step = Iteratee (return step)

yield :: Monad m => b -> Stream a -> Iteratee a m b
yield x extra = returnI (Yield x extra)

continue :: Monad m => (Stream a -> Iteratee a m b) -> Iteratee a m b
continue k = returnI (Continue k)

throwError :: (Monad m, Exc.Exception e) => e -> Iteratee a m b
throwError exc = returnI (Error (Exc.toException exc))

-- Iteratee a m is Applicative
instance Monad m => Applicative (Iteratee a m) where 
  (<*>) = ap
  pure = return

-- Iteratee a m is Monad
instance Monad m => Monad (Iteratee a m) where
  return x = yield x (Chunks [])
  m0 >>= f = ($ m0) $
    fix $ \bind m ->
            Iteratee $
            runIteratee m >>= \r1 ->
                                case r1 of
                                  Continue k -> return (Continue (bind . k))
                                  Yield x (Chunks []) -> runIteratee (f x)
                                  Yield x extra ->
                                    runIteratee (f x) >>= \r2 ->
                                      case r2 of
                                        Continue k -> runIteratee (k extra)
                                        Yield x' _ -> return (Yield x' extra)
                                        Error err -> return (Error err)
                                  Error err -> return (Error err)
  

-- Combinators
infixl 1 >>==
(>>==) :: Monad m
       => Iteratee a m b
       -> (Step a m b -> Iteratee a' m b')
       -> Iteratee a' m b'
i >>== f = Iteratee (runIteratee i >>= runIteratee . f)

joinI :: Monad m => Iteratee a m (Step a' m b) -> Iteratee a m b
joinI outer = outer >>= check where
  check (Continue k) = k EOF >>== \s -> case s of
                                          Continue _ -> error "joinI: divergent iteratee"
                                          _ -> check s
  check (Yield x _) = return x
  check (Error e) = throwError e

instance MonadTrans (Iteratee a) where
  lift m = Iteratee (m >>= runIteratee . return)

instance MonadIO m => MonadIO (Iteratee a m) where
  liftIO = lift . liftIO

-- run :: Monad m => Iteratee a m b -> m (Either Exc.SomeException b)
-- run i = do
--   mStep <- runIteratee $ enumEOF ==<< i
--   case mStep of
--     Error err -> return $ Left err
--     Yield x _ -> return $ Right x
--     Continue _ -> error "run: divergent iteratee"

-- run_ :: Monad m => Iteratee a m b -> m b
-- run_ i = run i >>= either Exc.throw return

-- type Enumerator a m b = Step a m b -> Iteratee a m b

-- enumList :: Monad m => Int -> [a] -> Enumerator a m b
-- enumList n = loop
--   where loop xs (Continue k)
--           | not (null xs) =
--             let (s1, s2) = splitAt n xs
--             in k (Chunks s1) >>== loop s2
--         loop _ step = returnI step

type Enumeratee ao ai m b = Step ai m b -> Iteratee ao m (Step ai m b)
-- 输入 Step ai m b
-- 得到（内部是 Stream ao 的）一系列 Step ai m b
-- 所以相当于把内部的流变掉了，但是 b 本身却没变
-- 类比 (i->b) -> [o->b]

checkYield :: Monad m
  -- |将流提升为Iteratee的智能构造器|  -> |新的Iteratee            |
  => ((Stream i -> Iteratee i m a) -> Iteratee o m (Step i m a))
  -- | Enumeratee |
  -> Enumeratee o i m a
checkYield _ y@(Yield x chunk) = return y
checkYield f (Continue k) = f k

iterateeMap :: Monad m => (o -> i) -> Enumeratee o i m a
iterateeMap f = checkYield $ continue . step
  where step k EOF = yield (Continue k) EOF
        step k (Chunks []) = continue $ step k
        step k chunk = k (fmap f chunk) >>== iterateeMap f

-- (=$) :: Monad m => Enumeratee ao ai m b -> Iteratee ai m b -> Iteratee ao m b
-- enum =$ iter = joinI (enum ==<< iter)
