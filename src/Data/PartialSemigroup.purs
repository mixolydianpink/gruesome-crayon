module Data.PartialSemigroup (class PartialSemigroup, pappend, (<>?)) where

import Prelude

import Data.Maybe (Maybe)

class PartialSemigroup a where
  pappend :: a -> a -> Maybe a

infix 5 pappend as <>?

instance partialSemigroupUnit :: PartialSemigroup Unit where
  pappend _ _ = pure unit
