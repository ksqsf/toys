import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Char

push :: Int -> State [Int] ()
push x = state $ \xs -> ((), x:xs)

pop :: State [Int] Int
pop = state $ \(x:xs) -> (x, xs)

pushMS :: Int -> MaybeT (State [Int]) ()
pushMS = lift . push

popMS :: MaybeT (State [Int]) Int
popMS = lift pop
