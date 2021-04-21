module Interpreter where

import Data.Maybe

type Prog = ([FDef], Expr)

type FDef = (String, ([String],Expr))

data Val
  = IVal { getInt :: Int }
  | BVal { getBool :: Bool }
  deriving (Show)

data Expr
  = Const { getVal :: Val }
  | Var String
  | Apply String [Expr]
  | Prim Op [Expr]
  | If Expr Expr Expr

data Op = Equal | Add | Sub | Mul

prog_exp = ("exp", (["x", "n"],
                    If (Prim Equal [Var "n", Const (IVal 0)])
                    (Const (IVal 1))
                    (Prim Mul [Var "x",
                               Apply "exp" [Var "x", Prim Sub [Var "n", Const (IVal 1)]]])))


type Env = [(String, Val)]

-- Interpreter
eval :: Prog -> Val
eval (fdefs, main) = eval' main []
  where
    eval' :: Expr -> Env -> Val
    eval' (Const v) env = v
    eval' (Var s) env =
      case lookup s env of
        Just v -> v
        Nothing -> error "undefined variable"
    eval' (Prim op es) env =
      let rs = [ eval' e env | e <- es ] in
        prim op rs
    eval' (If e0 e1 e2) env =
      if getBool (eval' e0 env)
      then eval' e1 env
      else eval' e2 env
    eval' (Apply f es) env =
      eval' body env'
      where
        (ss, body) = fromJust (lookup f fdefs)
        env' = zip ss [eval' e env | e<-es]
    prim Equal [IVal i1, IVal i2] = BVal (i1 == i2)
    prim Add [IVal i1, IVal i2] = IVal (i1 + i2)
    prim Sub [IVal i1, IVal i2] = IVal (i1 - i2)
    prim Mul [IVal i1, IVal i2] = IVal (i1 * i2)
