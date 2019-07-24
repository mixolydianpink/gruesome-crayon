module Data.PosInt (PosInt, fromInt, toInt) where

import Prelude

import Data.Enum (class Enum, pred, succ)
import Data.Maybe (Maybe(..))
import Data.Monogenous (class Monogenous)
import Data.PartialSemigroup (class PartialSemigroup)

newtype PosInt = PosIntImpl Int
derive newtype instance showPosInt :: Show PosInt
derive newtype instance eqPosInt :: Eq PosInt
derive newtype instance ordPosInt :: Ord PosInt

instance enumPosInt :: Enum PosInt where
  succ (PosIntImpl x) = map PosIntImpl (succ x)
  pred (PosIntImpl x) = map PosIntImpl $ pred x >>= (\x' -> if x' > 0 then Just x' else Nothing)

instance boundedPosInt :: Bounded PosInt where
  top = PosIntImpl top
  bottom = PosIntImpl 1

instance partialSemigroupPosInt :: PartialSemigroup PosInt where
  -- Extremely inefficient, but more elegant than messing with overflows.
  pappend x y =
    let
      one' :: Maybe PosInt
      one' = pure bottom
      plus :: Maybe PosInt -> Maybe PosInt -> Maybe PosInt
      plus x' y' = if x' == one' then y' >>= succ else plus (x' >>= pred) (y' >>= succ)
      plus' = if x <= y then plus else (flip plus)
    in plus' (pure x) (pure y)

instance monogenousPosInt :: Monogenous PosInt where
  generator = bottom

fromInt :: Int -> Maybe PosInt
fromInt x = if x > 0 then Just $ PosIntImpl x else Nothing

toInt :: PosInt -> Int
toInt (PosIntImpl x) = x
