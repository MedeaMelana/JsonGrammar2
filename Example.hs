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
  }

data Gender = Male | Female

cPerson :: Grammar c
  (Text :- Gender :- Int :- Float :- Float :- t)
  (Person :- t)
cPerson = Pure f g
  where
    f (a :- b :- c :- d :- e :- t) = Just (Person a b c d e :- t)
    g (Person a b c d e :- t) = Just (a :- b :- c :- d :- e :- t)

cMale :: Grammar c t (Gender :- t)
cMale = Pure f g
  where
    f t = Just (Male :- t)
    g (Male :- t) = Just t
    g _ = Nothing

cFemale :: Grammar c t (Gender :- t)
cFemale = Pure f g
  where
    f t = Just (Female :- t)
    g (Female :- t) = Just t
    g _ = Nothing

gGender :: Grammar Val (Value :- t) (Gender :- t)
gGender =  cMale   . Literal "male"
        <> cFemale . Literal "female"

gPerson :: Grammar Val (Value :- t) (Person :- t)
gPerson = cPerson . Object
  ( Property "name" gText 
  . Property "gender" gGender 
  . Property "age" gInt
  . Property "lat" gFloat
  . Property "lng" gFloat
  )
