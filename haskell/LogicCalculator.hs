{-# LANGUAGE GADTs #-}

data Formula ts where
  Body :: Term Bool -> Formula ()
  Forall :: Show a => [a] -> (Term a -> Formula as) -> Formula (a, as)
  Exist :: Show a => [a] -> (Term a -> Formula as) -> Formula (a, as)

data Term t where
  Con :: a -> Term a
  :&: :: Term Bool -> Term Bool -> Term Bool
  :|: :: Term Bool -> Term Bool -> Term Bool
  :<: :: Term Int -> Term Int -> Term Bool
  :>: :: Term Int -> Term Int -> Term Bool
  :=: :: Term Int -> Term Int -> Term Bool
  :+: :: Term Int -> Term Int -> Term Int
  :-: :: Term Int -> Term Int -> Term Int
  Name :: String -> Term t

eval :: Term t -> t
eval (Con v) = v
eval (p :&: q) = (eval p) && (eval q)
eval (p :|: q) = (eval p) || (eval q)
eval (n :<: m) = (eval n) < (eval m)
eval (n :=: m) = (eval n) == (eval m)
eval (n :+: m) = (eval n) + (eval m)
eval (n :-: m) = (eval n) - (eval m)
eval (Name _) = error "Cannot eval a Name"

satisfiable :: Formula ts -> Bool
satisfiable (Body body) = eval body
satisfiable (Forall xs as) = and [satisfiable (as (Con y))| y <- xs]
satisfiable (Exist  xs as) = or  [satisfiable (as (Con y))| y <- xs]
