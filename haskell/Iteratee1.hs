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
  Chunks xs >>= f = mconcat (fmap f xs)
  EOF >>= _ = EOF

instance Applicative Stream where
  pure = return
  (<*>) = ap

data Step a m b = Continue (Stream a -> Iteratee a m b)
                | Yield b (Stream a)
                | Error Exc.SomeException
                deriving (Functor)

newtype Iteratee s m a = Iteratee { runIteratee :: m (Step s m a) } deriving (Functor)

returnI :: Monad m => Step a m b -> Iteratee a m b
returnI step = Iteratee (return step)

yield :: Monad m => b -> Stream a -> Iteratee a m b
yield x extra = returnI (Yield x extra)

continue :: Monad m => (Stream a -> Iteratee a m b) -> Iteratee a m b
continue k = returnI (Continue k)

throwError :: (Monad m, Exc.Exception e) => e -> Iteratee a m b
throwError exc = returnI (Error (Exc.toException exc))


