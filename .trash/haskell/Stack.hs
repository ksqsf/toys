{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
import Control.Monad.State

type Stack a = [a]

push :: a -> State (Stack a) ()
push x = state $ \xs -> ((), x:xs)

pop :: State (Stack a) a
pop = state $ \(x:xs) -> (x, xs)

test = do
  pop

f = do
  push 5
  a <- pop
  push (a+5)

addStack :: State (Stack Int) ()
addStack = do
  a1 <- pop
  a2 <- pop
  let a3 = a1 + a2
  push a3

