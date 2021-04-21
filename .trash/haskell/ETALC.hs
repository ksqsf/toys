{-# LANGUAGE DeriveFunctor, FlexibleInstances, FlexibleContexts, OverlappingInstances, TypeOperators, MultiParamTypeClasses, RankNTypes #-}
-- # Error types á la carte
--
-- "Data types á la carte" provides a simple way to achieve an "open"
-- type, which can be extended quite easily.
--
-- This file attemps this method on "error" types. Errors are
-- notoriously hard, because ease of use and extensibily are
-- conflicting.

import Prelude hiding (userError)
import Control.Monad
import Data.Either

---
-- Combine...
---
infixr 6 :+:
data (f :+: g) e = Inl (f e) | Inr (g e) deriving Functor

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a

instance Functor f => f :<: f where
  inj = id

instance (Functor f, Functor g) => f :<: (f :+: g) where
  inj = Inl

instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = Inr . inj

--
-- Mother error types...
--
data Error f = In (f (Error f))

-- takeOut :: Error f -> f (Error f)
-- takeOut (In t) = t

inject :: (g :<: f) => g (Error f) -> Error f
inject = In . inj

foldError :: Functor f => (f a -> a) -> Error f -> a
foldError f (In t) = f (fmap (foldError f) t)

injectError :: (g :<: f) => Error g -> Error f
injectError = foldError inject

--
-- Operators...
--
class Functor f => Report f where
  reportAlgebra :: Report g => f (Error g) -> String
instance (Report f, Report g) => Report (f :+: g) where
  reportAlgebra (Inl x) = reportAlgebra x
  reportAlgebra (Inr y) = reportAlgebra y

report :: Report f => Error f -> String
report (In t) = reportAlgebra t

--
-- Error types...
--
data Success f = Success deriving (Show, Functor)
data Result a f = Result a deriving (Show, Functor)
data BadCode f = BadCode Int deriving (Show, Functor)
data UserError f = UserError String deriving (Show, Functor)

instance Report Success where
  reportAlgebra Success = "Success"
instance Report BadCode where
  reportAlgebra (BadCode x) = "Bad code (" ++ (show x) ++ ")"
instance Report UserError where
  reportAlgebra (UserError msg) = "User error: " ++ msg
instance Show a => Report (Result a) where
  reportAlgebra (Result x) = "Result: " ++ show x

success :: (Success :<: f) => Error f
success = inject Success

badCode :: (BadCode :<: f) => Int -> Error f
badCode code = inject (BadCode code)

userError :: (UserError :<: f) => String -> Error f
userError msg = inject (UserError msg)

result :: (Result a :<: f) => a -> Error f
result r = inject (Result r)

test1 :: Error (Success :+: BadCode)
test1 = success

--
-- Monads?...
---
-- Looks like a dead end... Use Either instead.
--
-- newtype MaybeError g a = MaybeError (Error ((Result a) :+: g))
-- instance Functor g => Functor (MaybeError g) where
--   -- f : a -> b
--   -- (MaybeError x) : MaybeError g a
--   -- x : Error ((Result a) :+: g)
--   -- res : MaybeError g b
--   fmap f (MaybeError (In x)) = MaybeError (In (fmap f x))
-- instance (Functor g) => Monad (MaybeError g) where
--   return x = MaybeError $ inject (Result x)
-- instance (Functor g) => Applicative (MaybeError g) where
--   pure = return
--   (<*>) = ap

--
-- Examples...
--
checkInt :: Int -> Either (Error BadCode) Int
checkInt x = if x < 0 then Left (badCode x) else Right x

checkString :: String -> Either (Error UserError) Int
checkString s = if length s > 10 then Left (userError "string too long") else Right (length s)

job :: Int -> String -> Either (Error (BadCode :+: UserError)) Int
job x y = do
  x <- from checkInt x
  y <- from checkString y
  return (x+y)

--
-- Helpers
--
liftEither :: (g :<: f) => Either (Error g) a -> Either (Error f) a
liftEither (Left x) = Left (injectError x)
liftEither (Right y) = Right y

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left a) = Left (f a)
mapLeft f (Right a) = Right a

from :: (g :<: f) => (a -> Either (Error g) b) -> a -> Either (Error f) b
from f x = liftEither (f x)
