{-# LANGUAGE GADTs, KindSignatures, DeriveFunctor, StandaloneDeriving, FlexibleContexts, UndecidableInstances, RankNTypes #-}

import Control.Applicative
import Control.Monad

data Free f a = Pure a | Free (f (Free f a)) deriving (Functor)

deriving instance (Show (f (Free f a)), Show a) => Show (Free f a)
deriving instance (Eq (f (Free f a)), Eq a) => Eq (Free f a)

instance Functor f => Monad (Free f) where
  return a = Pure a
  Pure a >>= k = k a
  Free m >>= k = Free ((>>= k) <$> m)

instance Functor f => Applicative (Free f) where
  pure = Pure
  (<*>) = ap

data InteractionOp :: * -> * where
  Say :: String -> (() -> r) -> InteractionOp r
  Ask :: (String -> r) -> InteractionOp r

deriving instance Functor InteractionOp

type Interaction = Free InteractionOp

say :: String -> Interaction ()
say msg = Free (Say msg Pure)

ask :: Interaction String
ask = Free (Ask Pure)

test1 :: Interaction ()
test1 = do
  say "who are you"
  a <- ask
  say $ "hello " ++ a

run1 :: InteractionOp a -> IO a
run1 (Say msg k) = putStrLn msg >>= \_ -> return (k ())
run1 (Ask fstr) = do l <- getLine; return (fstr l)

free :: (Functor f, Monad g) => (forall a.      f a -> g a) ->
                                (forall a. Free f a -> g a)
free f (Pure a) = return a
free f (Free fa) = join (f (fmap (free f) fa))
