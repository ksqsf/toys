import Control.Monad.State
import qualified Data.Map.Strict as Map

data Type = Int | Bool | Func Type Type | Var Char deriving (Show, Eq)
type TypeEnv = Map.Map Char Type

-- |Simplification (best-effort).
simplify :: TypeEnv -> Type -> Type
simplify _ Int = Int
simplify _ Bool = Bool
simplify env (Func a b) = Func (simplify env a) (simplify env b)
simplify env (Var c) = case Map.lookup c env of
                         Nothing -> Var c
                         Just t -> t

-- |Unification.
--  Returns the most concrete type and the resulted typing environment.
unify :: TypeEnv -> Type -> Type -> (TypeEnv, Type)
unify env Int Int = (env, Int)
unify env Bool Bool = (env, Bool)
unify env (Func a1 b1) (Func a2 b2) =
  let (env', a) = unify env a1 a2
      (env'', b) = unify env' b1 b2
  in (env'', simplify env'' (Func a b))
unify env (Var c) ty =
  let ty' = simplify env ty in
    case (Map.lookup c env) of
      Nothing -> if ty' /= (Var c)
                 then (Map.insert c ty' env, ty')
                 else (env, simplify env ty')
      Just tt -> let tt' = simplify env tt
                 in if ty' == tt' then (env, simplify env ty') else error "type error"
unify env ty (Var c) = unify env (Var c) ty


-- Func Int (Func (Var 'b') Int)
ex1 = snd $ unify Map.empty (Func (Var 'a') (Func (Var 'b') (Var 'a'))) (Func Int (Func (Var 'b') Int))

-- Func Int (Func Bool Int)
ex2 = snd $ unify Map.empty (Func (Var 'a') (Func (Var 'b') (Var 'a'))) (Func Int (Func Bool Int))

-- Func Int (Func Bool Int)
ex3 = snd $ unify Map.empty (Func (Var 'a') (Func (Var 'b') (Var 'a'))) (Func Int (Func Bool (Var 'a')))

-- Func Int (Func Bool Int)
ex4 = snd $ unify Map.empty (Func (Var 'a') (Func (Var 'b') (Var 'a'))) (Func (Var 'a') (Func Bool Int))

-- wtf... bug!
ex5 = unify Map.empty (Func (Var 'a') (Var 'a')) (Var 'a')

-- at least let's do something sane...
-- this naive typeck cannot even unify 'a with 'b, because they are not the same Char.
-- consider using de Bruijn index.
