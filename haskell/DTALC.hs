{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE DeriveFunctor #-}
import Control.Monad

-- Expression Problem: Define a data type by cases, where one can add
-- new cases to the data type and new functions over the data type,
-- without recompiling existing code, and while retaining static type
-- safety.

-- data Expr = Val Int | Add Expr Expr

-- eval :: Expr -> Int
-- eval (Val x) = x

-- render :: Expr -> String
-- render (Val x) = show x
-- render (Add x y) = "(" ++ render x ++ " + " ++ render y ++ ")"

data Expr f = In (f (Expr f))

data Val e = Val Int deriving Show
type IntExpr = Expr Val

data Add e = Add e e deriving Show
type AddExpr = Expr Add

-- The key idea is to combine expressions by taking the coproduct of their signatures.
infixr 6 :+:
data (f :+: g) e = Inl (f e) | Inr (g e)

addExample :: Expr (Val :+: Add)
addExample = In (Inr (Add (In (Inl (Val 118))) (In (Inl (Val 1219)))))

instance Functor Val where
  -- fmap :: _ -> (Val a) -> (Val b)
  -- Note: a and b are free.
  fmap _ (Val x) = Val x

instance Functor Add where
  -- fmap :: (a -> b) -> (Add a) -> (Add b)
  -- (Add a) == (Add a a)
  fmap f (Add e1 e2) = Add (f e1) (f e2)

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Inl x) = Inl $ fmap f x
  fmap f (Inr y) = Inr $ fmap f y

-- f is called an algebra; it determines how the different
-- constructors of a data type affect the final outcome. It specifies
-- one step of recursion, turning a value of type f a into the desired
-- result a.
foldExpr :: Functor f => (f a -> a) -> Expr f -> a
foldExpr f (In t) = f (fmap (foldExpr f) t)

class Functor f => Eval f where
  evalAlgebra :: f Int -> Int

instance Eval Val where
  evalAlgebra (Val x) = x

instance Eval Add where
  evalAlgebra (Add x y) = x + y

instance (Eval f, Eval g) => Eval (f :+: g) where
  evalAlgebra (Inl x) = evalAlgebra x
  evalAlgebra (Inr y) = evalAlgebra y

eval :: Eval f => Expr f -> Int
eval expr = foldExpr evalAlgebra expr

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a
  prj :: sup a -> Maybe (sub a)

instance Functor f => f :<: f where
  inj = id
  prj = Just . id

instance (Functor f, Functor g) => f :<: (f :+: g) where
  inj = Inl
  prj (Inl x) = Just x
  prj (Inr y) = Nothing

instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = Inr . inj
  prj (Inr y) = prj y
  prj (Inl x) = Nothing

inject :: (g :<: f) => g (Expr f) -> Expr f
inject = In . inj

val :: (Val :<: f) => Int -> Expr f
val x = inject (Val x)

infixl 6 ⊕
(⊕) :: (Add :<: f) => Expr f -> Expr f -> Expr f
x ⊕ y = inject (Add x y)

data Mul x = Mul x x
instance Functor Mul where
  fmap f (Mul x y) = Mul (f x) (f y)
instance Eval Mul where
  evalAlgebra (Mul x y) = x * y

infixl 7 ⊗
(⊗) :: (Mul :<: f) => Expr f -> Expr f -> Expr f
x ⊗ y = inject (Mul x y)

class Render f where
  render :: Render g => f (Expr g) -> String

pretty :: Render f => Expr f -> String
pretty (In t) = render t

instance Render Val where
  render (Val i) = show i
instance Render Add where
  render (Add x y) = "(" ++ pretty x ++ " + " ++ pretty y ++ ")"
instance Render Mul where
  render (Mul x y) = "(" ++ pretty x ++ " * " ++ pretty y ++ ")"
instance (Render f, Render g) => Render (f :+: g) where
  render (Inl f) = render f
  render (Inr g) = render g

match :: (g :<: f) => Expr f -> Maybe (g (Expr f))
match (In t) = prj t

distr :: (Add :<: f, Mul :<: f) => Expr f -> Maybe (Expr f)
distr t = do
  Mul a b <- match t
  Add c d <- match b
  return (a ⊗ c ⊕ a ⊗ d)


-- Monads are notoriously hard to combine...  In general, a structure
-- is called 'free' when it is left-adjoint to a forgetful functor.
-- In this specific instance, the Term data type is a higher-order
-- functor that maps a functor f to the monad Term f. This Term
-- functor is left-adjoint to the forgetful functor from monads to
-- their underlying functors.
data Term f a =
    Pure a
  | Impure (f (Term f a))

instance Functor f => Functor (Term f) where
  fmap f (Pure x) = Pure (f x)
  fmap f (Impure t) = Impure (fmap (fmap f) t)
instance Functor f => Monad (Term f) where
  return x = Pure x
  (Pure x) >>= f = f x
  (Impure t) >>= f = Impure (fmap (>>= f) t)
instance Functor f => Applicative (Term f) where
  pure = return
  (<*>) = ap

data Zero a                     -- corresponds to Identity
data One a = One                -- corresponds to Maybe
data Const e a = Const e        -- corresponds to Either

-- All left-adjoint functors preserve coproducts. Computing the
-- coproduct of two free monads reduces to computing the coproduct of
-- their underlying functor.
data Incr t = Incr Int t deriving (Functor)
data Recall t = Recall (Int -> t) deriving (Functor)

minject :: (g :<: f) => g (Term f a) -> Term f a
minject = Impure . inj

incr :: (Incr :<: f) => Int -> Term f ()
incr i = minject (Incr i (Pure ())) -- result is Pure ()

recall :: (Recall :<: f) => Term f Int
recall = minject (Recall Pure)  -- continuation is Pure

tick :: Term (Recall :+: Incr) Int
tick = do y <- recall
          incr 1
          return y

foldTerm :: Functor f => (a -> b) -> (f b -> b) -> Term f a -> b
foldTerm pure imp (Pure x) = pure x
foldTerm pure imp (Impure t) = imp (fmap (foldTerm pure imp) t)

newtype Mem = Mem Int deriving Show

run :: Run f => Term f a -> Mem -> (a, Mem)
run = foldTerm (,) runAlgebra

class Functor f => Run f where
  runAlgebra :: f (Mem -> (a, Mem)) -> (Mem -> (a, Mem))

instance Run Incr where
  runAlgebra (Incr k r) (Mem i) = r (Mem (i + k))

instance Run Recall where
  runAlgebra (Recall r) (Mem i) = r i (Mem i)

instance (Run f, Run g) => Run (f :+: g) where
  runAlgebra (Inl r) = runAlgebra r
  runAlgebra (Inr r) = runAlgebra r

-- Though we can combine Free monads this way, we can't do it on all
-- monads... Free monads are special.

data Teletype a =
    GetChar (Char -> a)
--  | PutChar Char a
  deriving Functor

exec :: Exec f => Term f a -> IO a
exec = foldTerm return execAlgebra

class Functor f => Exec f where
  execAlgebra :: f (IO a) -> IO a

instance Exec Teletype where
  execAlgebra (GetChar f) = getChar >>= f
