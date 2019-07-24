module Main where

import Prelude

import Control.Bind (bindFlipped)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List.Lazy (List, cycle, fromFoldable, iterate, take)
import Data.List.ZipList (ZipList(..))
import Data.Maybe (Maybe)
import Data.Monogenous (class Monogenous, generator)
import Data.Newtype (unwrap)
import Data.PartialSemigroup (pappend)
import Data.PosInt (PosInt)
import Data.Traversable (class Foldable, traverse_)
import Effect (Effect)
import Effect.Console (logShow)

data FB a
  = Fizz
  | Buzz
  | FizzBuzz
  | Blop a

derive instance genericFB :: Generic (FB a) _

instance showFB :: Show a => Show (FB a) where
  show = genericShow

fizzBuzz :: forall s. Monogenous s => List (Maybe (FB s))
fizzBuzz = unwrap $
  f <$> rep [false, false, true]
    <*> rep [false, false, false, false, true]
    <*> (ZipList $ iterate (bindFlipped $ pappend generator) (pure generator))
  where
    rep :: forall f a. Foldable f => f a -> ZipList a
    rep = ZipList <<< cycle <<< fromFoldable
    f :: forall f a. Functor f => Boolean -> Boolean -> f a -> f (FB a)
    f true  false = map $ const Fizz
    f false true  = map $ const Buzz
    f true  true  = map $ const FizzBuzz
    f false false = map $ Blop

main :: Effect Unit
main = traverse_ logShow $ take 100 (fizzBuzz :: List (Maybe (FB PosInt)))
