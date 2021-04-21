{-# LANGUAGE RankNTypes #-}

module Getter where

import Control.Applicative
import Data.Functor.Contravariant

type NaiveGetter s a = s -> a

type Getting r s a = (a -> Const r a) -> s -> Const r s
-- type Getter s a = forall r. Getting r s a

type Getter s a = forall f. (Contravariant f, Functor f) => (a -> f a) -> s -> f a
