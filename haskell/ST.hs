import Data.STRef
import Control.Monad.ST
import Control.Monad.State

-- runST :: forall {a}. (forall s. ST s a) -> a
-- newSTRef :: forall {a} {s}. a -> ST s (STRef s a)

-- foo :: STRef s Bool
-- foo = runST $ newSTRef True

-- • Couldn't match type ‘s1’ with ‘s’
--   ‘s1’ is a rigid type variable bound by
--     a type expected by the context:
--       forall s1. ST s1 (STRef s Bool)
--   ‘s’ is a rigid type variable bound by
--     the type signature for:
--       foo :: forall s. STRef s Bool
--   Expected type: ST s1 (STRef s Bool)
--     Actual type: ST s1 (STRef s1 Bool)

factorial :: Int -> STRef s Int -> ST s Int
factorial n accRef = do
  numRef <- newSTRef n
  num <- readSTRef numRef
  if num < 1
    then readSTRef accRef
    else do acc <- readSTRef accRef
            writeSTRef accRef (acc * n)
            writeSTRef numRef (num - 1)
            factorial (num - 1) accRef

fact :: Int -> Int
fact n = runST $ do accRef <- newSTRef 1
                    factorial n accRef

factorial' :: Int -> State Int Int
factorial' n =
  do acc <- get
     if n == 0
       then return acc
       else do put (n*acc)
               factorial' (n-1)

fact' :: Int -> Int
fact' n = evalState (factorial' n) 1
