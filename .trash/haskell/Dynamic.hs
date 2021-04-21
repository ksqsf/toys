import Control.Applicative
import Control.Monad
import Data.Dynamic

matchZero :: Dynamic -> Maybe Int
matchZero d = do
  c <- (fromDynamic d :: Maybe Int)
  guard (c == 0)
  return 0

matchBool :: Dynamic -> Maybe Int
matchBool d = do
  c <- (fromDynamic d :: Maybe Bool)
  if c then return 1 else return 0

dynamicMatch :: Dynamic -> Maybe Int
dynamicMatch a = foldl (<|>) Nothing [matchZero a, matchBool a]
