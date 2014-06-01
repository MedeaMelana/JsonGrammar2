-- | JsonGrammar allows you to express a bidirectional mapping between Haskell datatypes and JSON ASTs in one go.
module Language.JsonGrammar (

    -- * The Aeson example
    -- $example

    -- * Types
    Grammar, Context(..), (:-)(..),

    -- * Elemental building blocks
    pure, many, literal, label, object, property, array, element, coerce,

    -- * Constructing grammars
    fromPrism, defaultValue,

    -- * Wrapping constructors
    nil, cons, tup2,

    -- * Type-directed grammars
    Json(..), el, prop,

    -- * Using grammars
    parseValue, unparseValue, interfaces, SomeGrammar(..)

  ) where

import Language.JsonGrammar.Grammar
import Language.JsonGrammar.Parser
import Language.JsonGrammar.Unparser
import Language.JsonGrammar.TypeScript

-- $example
--
-- Aeson provides this example datatype:
--
-- > data Person = Person
-- >     { name :: Text
-- >     , age  :: Int
-- >     } deriving Show
--
-- With these conversion functions:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > instance FromJSON Person where
-- >     parseJSON (Object v) = Person <$>
-- >                            v .: "name" <*>
-- >                            v .: "age"
-- >     -- A non-Object value is of the wrong type, so fail.
-- >     parseJSON _          = mzero
-- >
-- > instance ToJSON Person where
-- >     toJSON (Person name age) = object ["name" .= name, "age" .= age]
--
-- From JsonGrammar's point of view, the problem with writing the conversions this way is that the same thing is written down twice: from one conversion, one can figure out what the conversion in the opposite direction should look like.
--
-- In JsonGrammar, the conversion looks like this:
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- >
-- > deriveStackPrismsFor ["person"] ''Person
-- >
-- > instance Json Person where
-- >   grammar = fromPrism person . object (prop "name" . prop "age")
--
-- This expresses the conversion in both directions in one go. The resulting parser and serializer are each other's inverse by construction.
--
-- As a bonus, if you name your grammar, JsonGrammar will generate a TypeScript definition for you:
--
-- > instance Json Person where
-- >   grammar = label "Person" $
-- >     fromPrism person . object (prop "name" . prop "age")
--
-- This results in this TypeScript definition:
--
-- > interface Person {age : number ;name : string ;}
