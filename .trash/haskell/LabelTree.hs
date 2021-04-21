import Control.Monad.State

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Show)

-- 不需要输入，单纯地转移状态并产生输出
increase :: State Int Int
increase = state $ \n -> (n, n+1)

ntAux :: Tree a -> State Int (Tree (a, Int))

ntAux (Leaf a) = do
  nl <- increase
  return (Leaf (a, nl))

ntAux (Node l n r) = do
  nl <- increase
  lt <- ntAux l
  rt <- ntAux r
  return (Node lt (n, nl) rt)

labelTree t = evalState (ntAux t) 0

test :: Tree Int
test = Node (Node (Leaf 5) 3 (Leaf 2)) 7 (Leaf 9)
