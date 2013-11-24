{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Parser where

import HypeScript

import Data.Aeson (Value, Object, withObject, (.:))
import Data.Aeson.Types (Parser)

parseValue :: Grammar Val t1 t2 -> t1 -> Parser t2
parseValue g =
  case g of
    Object gProps -> \(obj :- t) -> withObject "object" (parseProperties gProps t) obj

parseProperties :: Grammar Obj t1 t2 -> t1 -> Object -> Parser t2
parseProperties Id x _ = return x
parseProperties (Property n g) x obj = do
  val <- obj .: n
  parseValue g (val :- x)
parseProperties (g :. h) x obj = do
  y <- parseProperties h x obj
  parseProperties g y obj
