{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.JsonGrammar.Grammar (
    Grammar(..), Context(..), (:-)(..),
    pure, many, literal, label, object, property, array, element, coerce,
    defaultValue,
    nil, cons, tup2,
    Json(..), el, prop
  ) where

import Prelude hiding (id, (.))
import Control.Applicative ((<$>))
import Control.Category (Category(..))
import Data.Aeson (Value, FromJSON(..), ToJSON(..))
import Data.Aeson.Types (Parser)
import Data.Monoid (Monoid(..))
import Data.Text (Text)
import Language.TypeScript (Type(..), PredefinedType(..))



-- Types


data h :- t = h :- t deriving Show
infixr 5 :-

data Context = Val | Obj | Arr

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

instance Category (Grammar c) where
  id = Id
  (.) = (:.)

instance Monoid (Grammar c t1 t2) where
  mempty = Empty
  mappend = (:<>)



-- Elemental building blocks


pure :: (t1 -> Parser t2) -> (t2 -> Maybe t1) -> Grammar c t1 t2
pure = Pure

many :: Grammar c t t -> Grammar c t t
many =  Many

literal :: Value -> Grammar Val (Value :- t) t
literal = Literal

label :: Text -> Grammar Val t1 t2 -> Grammar Val t1 t2
label = Label

object :: Grammar Obj t1 t2 -> Grammar Val (Value :- t1) t2
object = Object

property :: Text -> Grammar Val (Value :- t1) t2 -> Grammar Obj t1 t2
property = Property

array :: Grammar Arr t1 t2 -> Grammar Val (Value :- t1) t2
array = Array

element :: Grammar Val (Value :- t1) t2 -> Grammar Arr t1 t2
element = Element

coerce :: Type -> Grammar Val t1 t2 -> Grammar Val t1 t2
coerce = Coerce



-- Wrapping constructors


nil :: Grammar c t ([a] :- t)
nil = Pure f g
  where
    f t = return ([] :- t)
    g ([] :- t) = return t
    g _ = fail "expected []"

cons :: Grammar c (a :- [a] :- t) ([a] :- t)
cons = Pure f g
  where
    f (x :- xs :- t) = return ((x : xs) :- t)
    g ((x : xs) :- t) = return (x :- xs :- t)
    g _ = fail "expected (:)"

tup2 :: Grammar c (a :- b :- t) ((a, b) :- t)
tup2 = Pure f g
  where
    f (x :- y :- t) = return ((x, y) :- t)
    g ((x, y) :- t) = return (x :- y :- t)



-- Type-directed grammars


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


liftAeson :: (FromJSON a, ToJSON a) => Grammar c (Value :- t) (a :- t)
liftAeson = Pure f g
  where
    f (val :- t) = (:- t) <$> parseJSON val
    g (x :- t) = Just (toJSON x :- t)

prop :: Json a => Text -> Grammar Obj t (a :- t)
prop n = Property n grammar

el :: Json a => Grammar Arr t (a :- t)
el = Element grammar

defaultValue :: Eq a => a -> Grammar c t (a :- t)
defaultValue x = Pure f g
  where
    f t = return (x :- t)
    g (x' :- t) | x == x' = Just t
    g _ = Nothing
