{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module HypeScript where

import Prelude hiding (id, (.))
import Control.Applicative ((<$>))
import Control.Category (Category(..))
import Data.Aeson (Value, FromJSON(..), ToJSON(..))
import Data.Aeson.Types (Parser)
import Data.Monoid (Monoid(..))
import Data.Text (Text)



-- Types


data h :- t = h :- t deriving Show
infixr 5 :-

data Context = Val | Obj | Arr

data Grammar (c :: Context) t1 t2 where
  -- Any context
  Id :: Grammar c t t
  (:.) :: Grammar c t2 t3 -> Grammar c t1 t2 -> Grammar c t1 t3
  Empty :: Grammar c t1 t2
  (:<>) :: Grammar c t1 t2 -> Grammar c t1 t2 -> Grammar c t1 t2
  Pure :: (t1 -> Parser t2) -> (t2 -> Maybe t1) -> Grammar c t1 t2
  Many :: Grammar c t t -> Grammar c t t

  -- Value context
  Literal :: Value -> Grammar Val (Value :- t) t

  -- Object context
  Object :: Grammar Obj t1 t2 -> Grammar Val (Value :- t1) t2
  Property :: Text -> Grammar Val (Value :- t1) t2 -> Grammar Obj t1 t2

  -- Array context
  Array :: Grammar Arr t1 t2 -> Grammar Val (Value :- t1) t2
  Element :: Grammar Val (Value :- t1) t2 -> Grammar Arr t1 t2

instance Category (Grammar c) where
  id = Id
  (.) = (:.)

instance Monoid (Grammar c t1 t2) where
  mempty = Empty
  mappend = (:<>)



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



-- Typeclass Json


class Json a where
  grammar :: Grammar Val (Value :- t) (a :- t)

instance Json Text  where grammar = liftAeson
instance Json Int   where grammar = liftAeson
instance Json Float where grammar = liftAeson

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
