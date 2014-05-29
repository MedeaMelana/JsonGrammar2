{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

import Types

import Language.JsonGrammar

import Prelude hiding (id, (.))
import Control.Category (Category(..))
import Data.Aeson.Types (Value(Null), parseMaybe)
import Data.Monoid ((<>))
import Data.Piso (Piso, fromPiso)
import Data.Piso.TH (derivePisos)
import Data.Text (Text)
import Language.TypeScript (renderDeclarationSourceFile)

import Test.Framework (Test, defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertBool)

person :: Piso
  (Text :- Gender :- Int :- Coords :- t)
  (Person :- t)
person = fromPiso $(derivePisos ''Person)

coords :: Piso
  (Float :- Float :- t)
  (Coords :- t)
coords = fromPiso $(derivePisos ''Coords)

male :: Piso t (Gender :- t)
female :: Piso t (Gender :- t)
(male, female) = $(derivePisos ''Gender)

instance Json Gender where
  grammar = fromPiso male   . literal "male"
         <> fromPiso female . literal "female"

instance Json Person where
  grammar = label "Person" $
    fromPiso person . object
    ( prop "name"
    . prop "gender"
    . (prop "age" <> defaultValue 42)
    . fromPiso coords
    -- . prop "lat"
    -- . prop "lng"
    . property "coords" (array (el . el))
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

test1 :: Test
test1 = testCase "PersonList" (assertBool "" (checkInverse [alice, bob]))

test2 :: Test
test2 = testCase "PersonTuple" (assertBool "" (checkInverse (alice, bob)))

test3 :: Test
test3 = testCase "TypeScriptInterfaces" (printInterfaces [SomeGrammar personGrammar])

printInterfaces :: [SomeGrammar Val] -> IO ()
printInterfaces gs = putStrLn (renderDeclarationSourceFile (interfaces gs))

consList :: (forall t'. Grammar Val (Value :- t') (a :- t')) ->
            Grammar Val (Value :- t) ([a] :- t)
consList g = label "Node" (nilCase <> consCase)
  where
    nilCase = nil . literal Null
    consCase =
      cons . object
        ( property "head" g
        . property "tail" (consList g)
        )

main :: IO ()
main = defaultMain [test1, test2, test3]
