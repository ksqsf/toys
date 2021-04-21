{-# LANGUAGE NoMonadFailDesugaring #-}
import Control.Monad.State

data Op = Plus | Minus | Mult | Div deriving (Show, Eq)
type LitOp = Either Double Op
type Stack = [LitOp]

push :: LitOp -> State Stack ()
push a = state $ \xs -> ((), a:xs)

pop :: State Stack LitOp
pop = state $ \(x:xs) -> (x, xs)

evaluate :: State Stack Double
evaluate = do
  top <- pop
  case top of
    Left val -> return val
    Right op -> do
      b <- evaluate
      a <- evaluate
      case op of
        Plus -> return (a+b)
        Minus -> return (a-b)
        Mult -> return (a*b)
        Div -> return (a/b)
-- evaluate = pop >>= \top ->
--   case top of
--     Left val -> return val
--     Right op ->
--       evaluate >>= \b ->
--       evaluate >>= \a ->
--       case op of
--         Plus -> return (a+b)
--         Minus -> return (a-b)
--         Mult -> return (a*b)
--         Div -> return (a/b)
