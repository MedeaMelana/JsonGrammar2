{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Example where

import HypeScript
import Parser
import Unparser

import Prelude hiding (id, (.))
import Control.Category (Category(..))
import Data.Aeson (Value)
import Data.Aeson.Types (parseMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)

data Person = Person
  { name   :: Text
  , gender :: Gender
  , age    :: Int
  , coords :: Coords
  } deriving (Show, Eq)

data Coords = Coords { lat :: Float, lng :: Float }
  deriving (Show, Eq)

data Gender = Male | Female deriving (Show, Eq)

cPerson :: Grammar c
  (Text :- Gender :- Int :- Coords :- t)
  (Person :- t)
cPerson = Pure f g
  where
    f (a :- b :- c :- d :- t) = return (Person a b c d :- t)
    g (Person a b c d :- t) = return (a :- b :- c :- d :- t)

cCoords :: Grammar c
  (Float :- Float :- t)
  (Coords :- t)
cCoords = Pure f g
  where
    f (a :- b :- t) = return (Coords a b :- t)
    g (Coords a b :- t) = return (a :- b :- t)

cMale :: Grammar c t (Gender :- t)
cMale = Pure f g
  where
    f t = return (Male :- t)
    g (Male :- t) = Just t
    g _ = Nothing

cFemale :: Grammar c t (Gender :- t)
cFemale = Pure f g
  where
    f t = return (Female :- t)
    g (Female :- t) = Just t
    g _ = Nothing

instance Json Gender where
  grammar = cMale   . Literal "male"
         <> cFemale . Literal "female"

instance Json Person where
  grammar = cPerson . Object
    ( prop "name"
    . prop "gender"
    . prop "age"
    . cCoords
    . prop "lat"
    . prop "lng"
    )

alice :: Person
alice = Person "Alice" Female 21 (Coords 52 5)

bob :: Person
bob = Person "Bob" Male 22 (Coords 53 6)

test :: Bool
test = alice == alice'
  where
    Just (aliceJson :- ()) = unparseValue grammar (alice :- ())
    Just (alice' :- ()) = parseMaybe (parseValue grammar) (aliceJson :- ())
