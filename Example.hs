{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Example where

import HypeScript
import Parser

import Prelude hiding (id, (.))
import Control.Category (Category(..))
import Data.Aeson (Value)
import Data.Monoid ((<>))
import Data.Text (Text)

data Person = Person
  { name   :: Text
  , gender :: Gender
  , age    :: Int
  , lat    :: Float
  , lng    :: Float
  } deriving Show

data Gender = Male | Female deriving Show

cPerson :: Grammar c
  (Text :- Gender :- Int :- Float :- Float :- t)
  (Person :- t)
cPerson = Pure f g
  where
    f (a :- b :- c :- d :- e :- t) = return (Person a b c d e :- t)
    g (Person a b c d e :- t) = Just (a :- b :- c :- d :- e :- t)

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
    . prop "lat"
    . prop "lng"
    )
