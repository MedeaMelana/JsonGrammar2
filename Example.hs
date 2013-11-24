{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Example where

import HypeScript
import Parser

import Prelude hiding (id, (.))
import Control.Category (Category(..))
import Data.Aeson (Value)
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
cPerson = undefined

gGender :: Grammar Val (Value :- t) (Gender :- t)
gGender = undefined

cMale, cFemale :: Grammar c (Gender :- t) t
cMale = undefined
cFemale = undefined

gPerson :: Grammar Val (Value :- t) (Person :- t)
gPerson = cPerson . Object
  ( Property "name" gText 
  . Property "gender" gGender 
  . Property "age" gInt
  . Property "lat" gFloat
  . Property "lng" gFloat
  )
