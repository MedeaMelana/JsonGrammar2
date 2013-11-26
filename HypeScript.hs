{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module HypeScript where

import Prelude hiding (id, (.))
import Control.Applicative ((<$>))
import Control.Category (Category(..))
import Data.Aeson (Value, FromJSON(..), ToJSON(..))
import Data.Aeson.Types (parseMaybe)
import Data.Text (Text)

data h :- t = h :- t
infixr 5 :-

data Context = Val | Obj | Arr

data Grammar (c :: Context) t1 t2 where
  -- Any context
  Id :: Grammar c t t
  (:.) :: Grammar c t2 t3 -> Grammar c t1 t2 -> Grammar c t1 t3
  Pure :: (t1 -> Maybe t2) -> (t2 -> t1) -> Grammar c t1 t2

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

gText :: Grammar Val (Value :- t) (Text :- t)
gText = liftAeson

gInt :: Grammar Val (Value :- t) (Int :- t)
gInt = liftAeson

gFloat :: Grammar Val (Value :- t) (Float :- t)
gFloat = liftAeson

liftAeson :: (FromJSON a, ToJSON a) => Grammar c (Value :- t) (a :- t)
liftAeson = Pure f g
  where
    f (val :- t) = (:- t) <$> parseMaybe parseJSON val
    g (x :- t) = toJSON x :- t
