{-# LANGUAGE DeriveFunctor #-}
import Control.Applicative
import Control.Monad

powerSet = filterM (Just . even)

-- This definition is very good...
-- filterM :: Applicative m => (a -> m Bool) -> [a] -> m [a]
-- filterM p = foldr (\x -> liftA2 (\flg -> if flg then (x:) else id) (p x)) (pure [])


data Parser a = Parser { unParser :: String -> Maybe (a, String) }
  deriving (Functor)

instance Monad Parser where
  return a = Parser $ \x -> Just (a, x)
  ma >>= k = Parser $ \s -> case unParser ma s of
    Nothing -> Nothing
    Just (result, rest) -> (unParser $ k result) rest

instance Alternative Parser where
  empty = Parser $ \s -> Nothing
  a <|> b = Parser $ \s -> case unParser a s of
    Nothing -> unParser b s
    Just a' -> Just a'

instance Applicative Parser where
  pure = return
  a <*> b = ap a b

instance MonadPlus Parser
