-- http://okmij.org/ftp/tagless-final/JFP.pdf

module Tagless where

import Data.Functor.Identity

data Var = VZ | VS Var deriving (Show, Eq)
data Exp = V Var | B Bool | L Exp | A Exp Exp

-- Each variable is represented using a unayr de Bruijn index.
-- (λx.x) => A (L (V VZ)) (B True)

--
-- Untyped attempt
--

-- lookup0 (x:_) VZ = x
-- lookup0 (_:t) (VS v) = lookup0 t v

-- eval0 env (V v) = lookup0 env v
-- eval0 env (B b) = b
-- eval0 env (L e) = \x -> eval0 (x:env) e
-- eval0 env (A e1 e2) = (eval0 env e1) (eval0 env e2)

--
-- "Finally Tagless" proposal: encode programs using ordinary functions
--
varZ env = fst env
varS vp env = vp (snd env)
b bv env = bv
lam0 e env = \x -> e (x,env)
app0 e1 e2 env = (e1 env) (e2 env)

-- (λx.x)True
testf1 = app0 (lam0 varZ) (b True)
testf1r = testf1 ()

-- (λx y . y) True False
testf3 = app0 (lam0 (varS varZ)) (b True)
testf3r = testf3 (False,())

--
-- Abstract the Interpreter
--
class Symantics repr where
  int  :: Int -> repr Int
  bool :: Bool -> repr Bool

  lam :: (repr a -> repr b) -> repr (a -> b)
  app :: repr (a -> b) -> repr a -> repr b
  fix :: (repr a -> repr a) -> repr a

  add :: repr Int -> repr Int -> repr Int
  mul :: repr Int -> repr Int -> repr Int
  leq :: repr Int -> repr Int -> repr Bool
  if_ :: repr Bool -> repr a -> repr a -> repr a

instance Symantics Identity where
  int = pure . id
  bool = pure . id
  lam f = pure $ \a -> runIdentity $ f (pure a)
  app = (<*>)
  fix f = let x = f x in x
  add (Identity a) (Identity b) = pure (a + b)
  mul (Identity a) (Identity b) = pure (a * b)
  leq (Identity a) (Identity b) = pure (a <= b)
  if_ b t e = if (runIdentity b) then t else e

testpowfix () = lam (\x ->
                       fix (\self ->
                              lam (\n ->
                                     if_ (leq n (int 0))
                                         (int 1)
                                         (mul x (app self (add n (int (-1))))))))
testpowfix7 () = lam (\x -> app (app (testpowfix ()) x) (int 3))
