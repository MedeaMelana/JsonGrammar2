module Types where

import Data.Text (Text)

data Person = Person
  { name     :: Text
  , gender   :: Gender
  , age      :: Int
  , location :: Coords
  } deriving (Show, Eq)

data Coords = Coords { lat :: Float, lng :: Float }
  deriving (Show, Eq)

data Gender = Male | Female deriving (Show, Eq)
