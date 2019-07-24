module Data.Monogenous (class Monogenous, generator) where

import Prelude

import Data.PartialSemigroup (class PartialSemigroup)

class (PartialSemigroup a) <= Monogenous a where
  generator :: a

instance monogenousUnit :: Monogenous Unit where
  generator = unit
