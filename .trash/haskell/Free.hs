{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
import Data.Functor
import Control.Monad

--
-- Free monoid
--
ins1 :: x -> [x]
ins1 x = [x]

free1 :: Monoid b => (x -> b) -> [x] -> b
free1 f [] = mempty
free1 f (x:xs) = f x `mappend` free1 f xs


--
-- Free monad
--
data Free f a = Return a | Free (f (Free f a)) deriving (Functor)

instance Functor f => Monad (Free f) where
  return = Return
  (Return a) >>= k = k a
  (Free m) >>= k = Free ((>>= k) <$> m)

instance Functor f => Applicative (Free f) where
  pure = return
  (<*>) = ap

ins2 :: (Functor f) => f a -> Free f a
ins2 fa = Free (fmap Return fa)

free2 :: (Functor f, Monad g) =>
        (forall a. f a -> g a) ->
        (forall a . Free f a -> g a)
free2 k (Return a) = return a
free2 k (Free fa) = join (k (fmap (free2 k) fa))

--
-- Free Applicative
--
infixr 4 :$:
data FreeA f a = Pure a | forall b. f (b -> a) :$: FreeA f b

deriving instance (Functor f) => Functor (FreeA f)

instance Functor f => Applicative (FreeA f) where
  pure = Pure
  Pure g <*> y = fmap g y
  (h :$: as) <*> y = fmap uncurry h :$: ((,) <$> as <*> y)

ins3 :: (Functor f) => f a -> FreeA f a
ins3 fa = fmap const fa :$: Pure ()

free3 :: (Functor f, Applicative g) =>
        (forall a. f a -> g a) ->
        (forall a. FreeA f a -> g a)
free3 k (Pure a) = pure a
free3 k (h :$: as) = k h <*> free3 k as
