data Exp = Lit Integer
         | Sub Exp Exp
         | Add Exp Exp
         | Mul Exp Exp
         | Div Exp Exp

eval :: Exp -> Integer
eval (Lit i) = i
eval (Sub e1 e2) = eval e1 - eval e2
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = eval e1 `div` eval e2

evalSeq :: Maybe Integer -> (Integer -> Maybe Integer) -> Maybe Integer
evalSeq mi f = case mi of
  Nothing -> Nothing
  Just i  -> f i

mb = Just 5 `evalSeq` \n1 ->
     Just 6 `evalSeq` \n2 ->
     Just (n1 + n2)

safeEval (Add e1 e2) = do
  n1 <- safeEval e1
  n2 <- safeEval e2
  return (n1+n2)
safeEval (Sub e1 e2) = do
  n1 <- safeEval e1
  n2 <- safeEval e2
  return (n1-n2)
safeEval (Mul e1 e2) = do
  n1 <- safeEval e1
  n2 <- safeEval e2
  return (n1*n2)
safeEval (Div e1 e2) = do
  n1 <- safeEval e1
  n2 <- safeEval e2
  case n2 of
    0 -> Nothing
    n2 -> return (n1/n2)
