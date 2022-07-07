{-# LANGUAGE FlexibleInstances #-}

module Applicative.Apl1 where

import Control.Applicative (ZipList (ZipList), liftA2)
import Data.Foldable (Foldable (fold))
import Data.Monoid (Sum)
import GHC.Base (liftA)

instance Semigroup a => Semigroup (ZipList a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (ZipList a) where
  mempty = pure mempty
  mappend = liftA2 mappend
