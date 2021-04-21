{-# LANGUAGE FlexibleContexts #-}
import Data.List
import Control.Monad.Writer
import Control.Monad.State

type Name = Char
type Label = Int

data Exp = Val Int | Var Name | App Op Exp Exp deriving Show
data Op = Add | Sub | Mul | Div deriving (Show, Eq)

comexp :: Exp -> Code
comexp (Val int) = [PUSH int]
comexp (Var name) = [PUSHV name]
comexp (App op e1 e2) = comexp e1 ++ comexp e2 ++ [DO op]

data Prog = Assign Name Exp
          | If Exp Prog Prog
          | While Exp Prog
          | Seqn [Prog]
          deriving (Show)

factorial :: Int -> Prog
factorial n = Seqn [Assign 'A' (Val 1),
                    Assign 'B' (Val n),
                    While (Var 'B') (
                       Seqn [Assign 'A' (App Mul (Var 'A') (Var 'B')),
                             Assign 'B' (App Sub (Var 'B') (Val 1))])]

type Stack = [Int]
type Code = [Inst]
data Inst = PUSH Int
          | PUSHV Name
          | POP Name
          | DO Op
          | JUMP Label
          | JUMPZ Label
          | LABEL Label
          deriving (Show)

-- 保存标签状态以便跳转
-- 记录生成的机器指令
type WT a = WriterT Code (State Int) a

-- 返回跳转标签
fresh :: WT Int
fresh = WriterT $ state (\s -> ((s, mempty), s+1))

mlabel (Assign name expr) = do
  tell $ comexp expr  -- 求值
  tell [POP name]     -- 存储到 name 中
  -- tell 用来记录机器码到 writer 里面

mlabel (If expr prog1 prog2) = do
  n <- fresh
  m <- fresh
  tell $ comexp expr -- 求值 expr
  tell [JUMPZ n]     -- 当为假时跳转到 else
  mlabel prog1       -- then
  tell [JUMP m]      -- 跳出 if
  tell [LABEL n]     -- else 的 label
  mlabel prog2       -- else
  tell [LABEL m]     -- 离开 if 的 label

mlabel (While expr prog) = do
  n <- fresh
  m <- fresh
  tell [LABEL n]
  tell $ comexp expr
  tell [JUMPZ m]
  mlabel prog
  tell [JUMP n]
  tell [LABEL m]

mlabel (Seqn []) = do tell []
mlabel (Seqn (c:cs)) = do mlabel c
                          mlabel (Seqn cs)

comp :: Prog -> Code
comp prog = snd $ fst $ (runState $ runWriterT $ mlabel prog) 0

type Machine = (Code, Stack, Mem, Counter)
type Counter = Int
type Mem = [(Name, Int)]

execline :: Inst -> Machine -> Machine
execline (PUSH x) (c, s, m, pc) = (c, x:s, m, pc+1)
execline (POP v) (c, top:s, m, pc) = (c, s, defineVar m v top, pc+1)
execline (PUSHV v) (c, s, m, pc) = (c, (findVar m v):s, m, pc+1)
execline (DO op) (c, b:a:s, m, pc) = (c, r:s, m, pc+1)
  where r = case op of
              Add -> a+b
              Sub -> a-b
              Mul -> a*b
              Div -> a`div`b
execline (LABEL l) (c,s,m,pc) = (c, s, m, pc+1)
execline (JUMP d) (c,s,m,pc) = (c, s, m, findLabel c d)
execline (JUMPZ d) (c,top:s,m,pc) = (c,s,m,pc')
  where pc' = if top == 0 then findLabel c d else pc + 1

defineVar :: Mem -> Name -> Int -> Mem
defineVar [] v x = [(v,x)]
defineVar ((v',x'):s) v x = if v' == v then (v',x):s else (v',x'):defineVar s v x

findVar :: Mem -> Name -> Int
findVar [] v = error "no such var"
findVar ((v',x'):s) v = if v' == v then x' else findVar s v

findLabel' :: Int -> Code -> Label -> Int
findLabel' _ [] _ = error "no such label"
findLabel' n (LABEL l':cs) l | l == l' = n
findLabel' n (c:cs) l = findLabel' (n+1) cs l
findLabel :: Code -> Label -> Int
findLabel = findLabel' 0

machine :: Machine
machine = (comp (factorial 3), [], [], 0)
