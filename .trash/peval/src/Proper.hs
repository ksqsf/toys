module Proper where

import Data.Maybe
import Control.Monad.State.Lazy

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


-- Proper PEval specializes a whole program
peval :: Prog -> Prog
peval (fdefs, main) = swap (runState (peval' main []) [])
  where
    peval' :: Expr -> Env -> State [FDef] Expr
    peval' (Const v) env = return (Const v)
    peval' (Var s) env =
      case lookup s env of
        Just v -> return v
        Nothing -> return (Var s)
    peval' (Prim op es) env = do
      rs <- mapM (flip peval' env) es
      if all isVal rs
        then return (Const (prim op (map getVal rs)))
        else return (Prim op rs)
    peval' (Apply s es) env = do
      -- Look up function
      let (ss, body) = fromJust (lookup s fdefs)
      -- Partially evaluate arguments
      rs <- mapM (flip peval' env) es
      -- Determin static and dynamic arguments
      let z = zip ss rs
      let sas = [ (s, getVal r) | (s, r) <- z, isVal r ]
      let das = [ (s, v) | (s, v) <- z, not (isVal r) ]
      if null das
        then
          -- Inline completely static applications
          peval' body sas
        else do
          -- 
          let s' = s ++ show (hashString (show sas))
          -- Specialize each "name" just once
          fdefs <- get
          when (isNothing (lookup s' fdefs))
            (do -- Create place holder for memoization
                put (fdefs ++ [(s', undefined)])
                -- Partially evaluate function body
                e' <- peval' body sas
                -- Replace placeholder by actual definition
                modify (update (const (map fst das, e')) s'))
          -- return application of specialized function
          return (Apply s' (map snd das))
    peval' (If e0 e1 e2) env = do
      r0 <- peval' e0 env
      if isVal r0 then
        if getBool (getVal r0)
          then peval' e1 env
          else peval' e2 env
      else do
        r1 <- peval' e1 env
        r2 <- peval' e2 env
        return (If r0 r1 r2)
