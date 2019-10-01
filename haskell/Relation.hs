import Data.Monoid
import Control.Monad.Reader
import Control.Monad.State

(|>) :: Monoid a => a -> a -> a
(|>) = mappend

newtype FunApp s = FunApp { appFunApp :: s -> s }
instance Monoid (FunApp s) where
  mempty = FunApp id
  (FunApp f) `mappend` (FunApp g) = FunApp $ g . f

instance Semigroup (FunApp s) where
  (<>) = mappend

type Stack = [Int]

push :: Int -> FunApp Stack
push i = FunApp $ \xs -> i:xs

pop :: FunApp Stack
pop = FunApp $ \(x:xs) -> xs

m :: FunApp Stack
m = push 3
  |> push 1
  |> pop


