{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Example where

import Language.JsonGrammar

import Prelude hiding (id, (.))
import Control.Category (Category(..))
import Data.Aeson.Types (Value(Null), parseMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Language.TypeScript (renderDeclarationSourceFile)

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
  grammar = Label "Person" $
    cPerson . Object
    ( prop "name"
    . prop "gender"
    . (prop "age" <> defaultValue 42)
    . cCoords
    -- . prop "lat"
    -- . prop "lng"
    . Property "coords" (Array (el . el))
    )

personGrammar :: Grammar Val (Value :- t) (Person :- t)
personGrammar = grammar

alice :: Person
alice = Person "Alice" Female 21 (Coords 52 5)

bob :: Person
bob = Person "Bob" Male 22 (Coords 53 6)

checkInverse :: (Json a, Eq a) => a -> Bool
checkInverse value = value == value'
  where
    Just (json :- ()) = unparseValue grammar (value :- ())
    Just (value' :- ()) = parseMaybe (parseValue grammar) (json :- ())

test :: Bool
test = checkInverse [alice, bob] && checkInverse (alice, bob)

test2 :: IO ()
test2 = printInterfaces [SomeGrammar personGrammar]

printInterfaces :: [SomeGrammar Val] -> IO ()
printInterfaces gs = putStrLn (renderDeclarationSourceFile (interfaces gs))

consList :: (forall t'. Grammar Val (Value :- t') (a :- t')) ->
            Grammar Val (Value :- t) ([a] :- t)
consList g = Label "Node" (nilCase <> consCase)
  where
    nilCase = nil . Literal Null
    consCase =
      cons . Object
        ( Property "head" g
        . Property "tail" (consList g)
        )
