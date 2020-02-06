module Naive where

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
  deriving (Show)

data Op = Equal | Add | Sub | Mul
  deriving (Show)

prog_exp = ("exp", (["x", "n"],
                    If (Prim Equal [Var "n", Const (IVal 0)])
                    (Const (IVal 1))
                    (Prim Mul [Var "x",
                               Apply "exp" [Var "x", Prim Sub [Var "n", Const (IVal 1)]]])))


type Env = [(String, Expr)]     -- Naive, inlining-oriented partial evaluator

isVal :: Expr -> Bool
isVal (Const _) = True
isVal _ = False

peval :: Prog -> Expr
peval (fdefs, main) = peval' main []
  where
    peval' :: Expr -> Env -> Expr
    peval' (Const v) env = Const v
    peval' (Var s) env =
      case lookup s env of
        Just e -> e
        Nothing -> Var s
    peval' (Prim op es) env =
      let rs = [ peval' e env | e <- es ] in
        if all isVal rs
        then Const (prim op (map getVal rs))
        else Prim op rs
    peval' (If e0 e1 e2) env =
      let r0 = peval' e0 env in
        if isVal r0
        then if getBool (getVal r0)
             then peval' e1 env
             else peval' e2 env
        else (If r0
               (peval' e1 env)
               (peval' e2 env))
    peval' (Apply f es) env =
      peval' body env'
      where
        (ss, body) = fromJust (lookup f fdefs)
        env' = zip ss [peval' e env | e <- es]
    prim Equal [IVal i1, IVal i2] = BVal (i1 == i2)
    prim Add [IVal i1, IVal i2] = IVal (i1 + i2)
    prim Sub [IVal i1, IVal i2] = IVal (i1 - i2)
    prim Mul [IVal i1, IVal i2] = IVal (i1 * i2)
