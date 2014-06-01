{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

import Language.JsonGrammar
import Types

import Prelude hiding (id, (.))
import Control.Category (Category(..))
import Data.Aeson.Types (Value(Null), parseMaybe)
import Data.Char (toLower)
import Data.Monoid ((<>))
import Data.StackPrism (StackPrism)
import Data.StackPrism.TH (deriveStackPrismsWith)
import Data.Text (Text)
import Language.TypeScript (renderDeclarationSourceFile)
import Test.Framework (Test, defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertBool)


-- Create stack prisms for our three datatypes (defined in module Types)

deriveStackPrismsWith (map toLower) ''Person
deriveStackPrismsWith (map toLower) ''Coords
deriveStackPrismsWith (map toLower) ''Gender


-- Write Json instances for the datatypes

instance Json Gender where
  grammar = fromPrism male   . literal "male"
         <> fromPrism female . literal "female"

instance Json Person where
  grammar = label "Person" $
    fromPrism person . object
    ( prop "name"
    . prop "gender"
    . (prop "age" <> defaultValue 42)
    . fromPrism coords
    . property "coords" (array (el . el))
    )


-- Create two example values

alice :: Person
alice = Person "Alice" Female 21 (Coords 52 5)

bob :: Person
bob = Person "Bob" Male 22 (Coords 53 6)


-- Two tests: one for lists, the other for tuples

test1, test2 :: Test
test1 = testCase "PersonList"  $ assertBool "" (checkInverse [alice, bob])
test2 = testCase "PersonTuple" $ assertBool "" (checkInverse (alice, bob))

checkInverse :: (Json a, Eq a) => a -> Bool
checkInverse value = value == value'
  where
    Just (json   :- ()) = unparseValue grammar (value :- ())
    Just (value' :- ()) = parseMaybe (parseValue grammar) (json :- ())


-- Write the TypeScript definition to stdout and run the tests

main :: IO ()
main = do
  printInterfaces [SomeGrammar personGrammar]
  defaultMain [test1, test2]

personGrammar :: Grammar Val (Value :- t) (Person :- t)
personGrammar = grammar

printInterfaces :: [SomeGrammar Val] -> IO ()
printInterfaces gs = putStrLn (renderDeclarationSourceFile (interfaces gs))
