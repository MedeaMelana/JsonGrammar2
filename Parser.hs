{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Parser where

import HypeScript

import Control.Monad ((>=>))
import Data.Aeson (Value, Object, Array, withObject, (.:), withArray)
import Data.Aeson.Types (Parser, typeMismatch)

parseValue :: Grammar Val t1 t2 -> t1 -> Parser t2
parseValue g =
  case g of
    Id          -> return
    g :. h      -> parseValue h >=> parseValue g
    Literal val -> \(val' :- t) ->
      if val == val'
        then return t
        else typeMismatch "literal" val'
    Object gProps ->
      \(obj :- x) -> withObject "object" (parseProperties gProps x) obj
    Array gElements ->
      \(arr :- x) -> do
        (y, arr') <- withArray "array" (parseElements gElements x) arr
        return y

parseProperties :: Grammar Obj t1 t2 -> t1 -> Object -> Parser t2
parseProperties Id x _ = return x
parseProperties (g :. h) x obj = do
  y <- parseProperties h x obj
  z <- parseProperties g y obj
  return z
parseProperties (Property n g) x obj = do
  val <- obj .: n
  parseValue g (val :- x)

parseElements :: Grammar Arr t1 t2 -> t1 -> Array -> Parser (t2, Array)
parseElements Id x arr = return (x, arr)
parseElements (g :. h) x arr = do
  (y, arr') <- parseElements h x arr
  parseElements g y arr'
--parseProperties (Element g) x obj = do
--  val <- obj .: n
--  parseValue g (val :- x)

