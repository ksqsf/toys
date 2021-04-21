{-# LANGUAGE RankNTypes #-}

module Setter where

import Data.Functor.Identity
import Control.Lens (Settable)

-- A generialized fmap!
type NaiveSetter s t a b = (a -> b) -> s -> t

-- type Setter s t a b = (a -> Identity b) -> s -> Identity t
type Setter s t a b = forall f. Settable f => (a -> f b) -> s -> f t

