{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Language.JsonGrammar.Grammar (
    Grammar(..), Context(..), (:-)(..),
    pure, many, literal, label, object, property, array, element, coerce,
    fromPrism, defaultValue,
    nil, cons, tup2,
    Json(..), el, prop
  ) where

import Prelude hiding (id, (.))
import Control.Applicative ((<$>))
import Control.Category (Category(..))
import Data.Aeson (Value, FromJSON(..), ToJSON(..))
import Data.Aeson.Types (Parser)
import Data.Monoid (Monoid(..))
import Data.StackPrism (StackPrism, stackPrism, forward, backward, (:-)(..))
import Data.String (IsString(..))
import Data.Text (Text)
import Language.TypeScript (Type(..), PredefinedType(..))
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text

-- Types


-- | The context of a grammar. Most combinators ask for a grammar in a specific context as input, and produce a grammar in another context.
data Context
  = Val -- ^ Value context
  | Obj -- ^ Object context, for defining object members
  | Arr -- ^ Array context, for defining array elements

-- | A @Grammar@ provides a bidirectional mapping between a Haskell datatype and its JSON encoding. Its first type argument specifies its context: either it's defining properties (context 'Obj'), array elements (context 'Arr') or values (context 'Val').
data Grammar (c :: Context) t1 t2 where
  Id       :: Grammar c t t
  (:.)     :: Grammar c t2 t3 -> Grammar c t1 t2 -> Grammar c t1 t3

  Empty    :: Grammar c t1 t2
  (:<>)    :: Grammar c t1 t2 -> Grammar c t1 t2 -> Grammar c t1 t2

  Pure     :: (t1 -> Parser t2) -> (t2 -> Maybe t1) -> Grammar c t1 t2
  Many     :: Grammar c t t -> Grammar c t t

  Literal  :: Value -> Grammar Val (Value :- t) t

  Label    :: Text -> Grammar Val t1 t2 -> Grammar Val t1 t2

  Object   :: Grammar Obj t1 t2 -> Grammar Val (Value :- t1) t2
  Property :: Text -> Grammar Val (Value :- t1) t2 -> Grammar Obj t1 t2

  Array    :: Grammar Arr t1 t2 -> Grammar Val (Value :- t1) t2
  Element  :: Grammar Val (Value :- t1) t2 -> Grammar Arr t1 t2

  Coerce   :: Type -> Grammar Val t1 t2 -> Grammar Val t1 t2

-- | The '.' operator is the main way to compose two grammars.
instance Category (Grammar c) where
  id = Id
  (.) = (:.)

-- | The @Monoid@ instance allows you to denote choice: if the left grammar doesn't succeed, the right grammar is tried.
instance Monoid (Grammar c t1 t2) where
  mempty = Empty
  mappend = (:<>)

-- | String literals convert to grammars that expect or produce a specific JSON string 'literal' value.
instance IsString (Grammar Val (Value :- t) t) where
  fromString = literal . fromString



-- Elemental building blocks


-- | Creates a pure grammar that doesn't specify any JSON format but just operates on the Haskell level. Pure grammars can be used in any context.
pure :: (t1 -> Parser t2) -> (t2 -> Maybe t1) -> Grammar c t1 t2
pure = Pure

-- | Try to apply a grammar as many times as possible. The argument grammar's output is fed to itself as input until doing so again would fail. This allows you to express repetitive constructions such as array elements. 'many' can be used in any context.
many :: Grammar c t t -> Grammar c t t
many =  Many

-- | Expect or produce a literal JSON 'Value'. You can only use this constructor in the value context 'Val'.
literal :: Value -> Grammar Val (Value :- t) t
literal = Literal

-- | Label a value grammar with a name. This doesn't affect the JSON conversion itself, but it generates an interface definition when converting to TypeScript 'interfaces'.
label :: Text -> Grammar Val t1 t2 -> Grammar Val t1 t2
label = Label

-- | Expect or produce a JSON object whose contents match the specified 'Obj' grammar. You can create 'Obj' grammars using 'property'. Alternatively, if you want to match an empty object, use @object 'id'@.
object :: Grammar Obj t1 t2 -> Grammar Val (Value :- t1) t2
object = Object

-- | Expect or produce an object property with the specified name, and a value that can be parsed/produced by the specified grammar. This function creates a grammar in the 'Obj' context. You can combine multiple @property@ grammars using the '.' operator from 'Category'.
--
-- Use '<>' to denote choice. For example, if you are creating an object with a property called @"type"@, whose value determines what other properties your object has, you can write it like this:
--
-- > grammar = object (propertiesA <> propertiesB)
-- >   where
-- >     propertiesA = property "type" "A" . fromPiso constructorA . prop "foo"
-- >     propertiesB = property "type" "B" . fromPiso constructorB . prop "bar" . prop "baz"
property :: Text -> Grammar Val (Value :- t1) t2 -> Grammar Obj t1 t2
property = Property

-- | Expect or produce a JSON array value whose contents match the specified 'Arr' grammar. You can create 'Arr' grammars using 'element'. Alternatively, if you want to match an empty array, use @array 'id'@.
array :: Grammar Arr t1 t2 -> Grammar Val (Value :- t1) t2
array = Array

-- | Expect or produce a JSON array element whose value matches the specified 'Val' grammar.
element :: Grammar Val (Value :- t1) t2 -> Grammar Arr t1 t2
element = Element

-- | Mark a grammar to be of a specific TypeScript type. This doesn't affect the JSON conversion, but when generating TypeScript 'interfaces' a coercion causes the interface generator to stop looking at the underlying grammar and just use the specified TypeScript 'Type' as inferred type instead.
--
-- This is useful if you write a grammar that, for example, wraps a primitive type like string (in which case you would specify @'Predefined' 'StringType'@ as type). Another use is when you find the generated interface can't be described by a 'Grammar', for example because it uses a generic type parameter.
coerce :: Type -> Grammar Val t1 t2 -> Grammar Val t1 t2
coerce = Coerce



-- Wrapping constructors


-- | A 'pure' grammar that expects or produces the empty list @[]@.
nil :: Grammar c t ([a] :- t)
nil = Pure f g
  where
    f t = return ([] :- t)
    g ([] :- t) = return t
    g _ = fail "expected []"

-- | A 'pure' grammar that expects or produces a cons ':'.
cons :: Grammar c (a :- [a] :- t) ([a] :- t)
cons = Pure f g
  where
    f (x :- xs :- t) = return ((x : xs) :- t)
    g ((x : xs) :- t) = return (x :- xs :- t)
    g _ = fail "expected (:)"

-- | A 'pure' grammar that wraps or unwraps a tuple.
tup2 :: Grammar c (a :- b :- t) ((a, b) :- t)
tup2 = Pure f g
  where
    f (x :- y :- t) = return ((x, y) :- t)
    g ((x, y) :- t) = return (x :- y :- t)



-- Type-directed grammars


-- | A type class for types that can be converted from and to JSON using a 'Grammar'. The grammar is expected to be in the value context 'Val' and consumes (or produces) a JSON 'Value'.
class Json a where
  grammar :: Grammar Val (Value :- t) (a :- t)

instance Json Text  where grammar = Coerce (Predefined StringType) liftAeson
instance Json Int   where grammar = Coerce (Predefined NumberType) liftAeson
instance Json Float where grammar = Coerce (Predefined NumberType) liftAeson

instance Json a => Json [a] where
  grammar = Array (Many (Element (cons . grammar)) . nil)

instance (Json a, Json b) => Json (a, b) where
  grammar = tup2 . Array (Element grammar . Element grammar)



-- Constructing grammars


-- | Create a 'pure' grammar for a type that aeson already knows how to convert from/to JSON.
liftAeson :: (FromJSON a, ToJSON a) => Grammar c (Value :- t) (a :- t)
liftAeson = Pure f g
  where
    f (val :- t) = (:- t) <$> parseJSON val
    g (x :- t) = Just (toJSON x :- t)

-- | Expect or produce an object 'property' whose value grammar is specified by 'grammar'.
prop :: Json a => Text -> Grammar Obj t (a :- t)
prop n = Property n grammar

-- | Expect or produce an array 'element' whose value grammar is specified by 'grammar'.
el :: Json a => Grammar Arr t (a :- t)
el = Element grammar

-- | Create a 'pure' grammar that expects or produces a specific Haskell value.
defaultValue :: Eq a => a -> Grammar c t (a :- t)
defaultValue x = Pure f g
  where
    f t = return (x :- t)
    g (x' :- t) | x == x' = Just t
    g _ = Nothing

-- | Create a 'pure' grammar from a 'StackPrism'.
fromPrism :: StackPrism a b -> Grammar c a b
fromPrism p = Pure (return . forward p) (backward p)

-- | Apply a prism to the top of the stack
--
-- TODO: It would be nicer if this was part of Data.StackPrism
top :: StackPrism a b -> StackPrism (a :- t) (b :- t)
top prism = stackPrism (\(a :- t) -> (forward prism a :- t))
                       (\(b :- t) -> (:- t) `fmap` backward prism b)

-- | Grammar for strings
--
-- (Defined explicitly rather than a Json instance so that we do not rely
-- on OverlappingInstances.)
string :: Grammar Val (Value :- t) (String :- t)
string = fromPrism (top (stackPrism Text.unpack (Just . Text.pack))) . grammar
