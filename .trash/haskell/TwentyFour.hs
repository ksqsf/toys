import Data.List (permutations)

data Exp = Val Double   -- 0
         | Plus Exp Exp -- 1
         | Sub  Exp Exp -- 1
         | Mult Exp Exp -- 2
         | Div  Exp Exp -- 2
         deriving (Show, Eq)

eval :: Exp -> Double
eval (Val a) = a
eval (Plus a b) = eval a + eval b
eval (Sub a b) = eval a - eval b
eval (Mult a b) = eval a * eval b
eval (Div a b) = eval a / eval b

showExp' :: Int -> Exp -> String

showExp' _ (Val a) = show a

showExp' 1 (Plus a b) = "("++showExp' 1 a++"+"++showExp' 1 b++")"
showExp' _ (Plus a b) = showExp a++"+"++showExp b

showExp' 1 (Sub a b) = "("++showExp' 1 a++"-"++showExp' 1 b++")"
showExp' _ (Sub a b) =  showExp' 1 a++"-"++showExp' 1 b

showExp' _ (Mult a b) = showExp' 2 a++"*"++showExp' 2 b

showExp = showExp' 3

divide :: [a] -> [([a], [a])]
divide xs = [ (take n xs, drop n xs) | n <- [1..(length xs - 1)] ]

buildExpressions :: ([Exp], [Exp]) -> [Exp]
buildExpressions (es1, es2) =
  [op e1 e2 | e1 <- es1, e2 <- es2, op <- [Plus, Sub, Mult, Div]]

toExpressions :: [Double] -> [Exp]
toExpressions [] = []
toExpressions [x] = [Val x]
toExpressions xs = concat
  [buildExpressions (toExpressions l, toExpressions r) | (l, r) <- divide xs]

generate :: [Double] -> [Exp]
generate ns = concatMap toExpressions (permutations ns)

twentyfour :: [Double] -> [String]
twentyfour ns = [ showExp x | x <- generate ns, eval x == 24.0 ]
