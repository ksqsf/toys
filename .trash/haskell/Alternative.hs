{-# LANGUAGE DeriveFunctor #-}

import Control.Applicative
import Data.Char

-- class Applicative f => Alternative f where
--   empty :: f a
--   (<|>) :: f a -> f a -> f a

-- instance Alternative [] where
--   empty = []
--   (<|>) = (++)

-- A function that takes a String and returns the result when successful.
newtype Parser a = Parser
  { runParser :: String -> Maybe (a, String)
  } deriving (Functor)
-- newtype instead of type, because ((->) r) itself may conflict with it.

-- instance Functor Parser where
--   -- (a->b) -> Parser a -> Parser b
--   fmap f p = Parser $ \str ->
--     case runParser p str of
--       Just (a,s) -> Just (f a, s)
--       Nothing -> Nothing

instance Applicative Parser where
  pure a = Parser $ \str -> Just (a, str)
  -- Parser (a -> b) -> Parser a -> Parser b
  (<*>) fp a = Parser $ \str ->
    case runParser fp str of
      Nothing -> Nothing
      Just (ab, s) ->
        case runParser a s of
          Nothing -> Nothing
          Just (at, s1) -> Just (ab at, s1)

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (<|>) a b = Parser $ \str ->
    case runParser a str of
      Nothing -> runParser b str
      just -> just

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \str -> case str of
  [] -> Nothing
  s:ss -> if f s then Just (s, ss) else Nothing

char :: Char -> Parser Char
char c = satisfy (==c)

sequ :: Parser a -> Parser [a] -> Parser [a]
sequ = liftA2 (:)

number :: Parser Int
number = (foldl (\x y -> 10*x+y) 0) <$> (some digit)
  where digit = digitToInt <$> (satisfy isDigit)

parseStr :: [Char] -> Parser [Char]
parseStr strs = foldr sequ (Parser $ \str -> Just ([], str)) [char s|s<-strs]
