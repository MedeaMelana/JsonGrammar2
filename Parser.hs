{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Parser where

import HypeScript

import Control.Monad ((>=>), unless)
import Data.Aeson (Object, Array, withObject, (.:), withArray)
import Data.Aeson.Types (Parser, typeMismatch)
import qualified Data.Vector as V

parseValue :: Grammar Val t1 t2 -> t1 -> Parser t2
parseValue g =
  case g of
    Id          -> return
    g1 :. g2      -> parseValue g2 >=> parseValue g1
    Literal val -> \(val' :- t) ->
      if val == val'
        then return t
        else typeMismatch "literal" val'
    Object gProps ->
      \(obj :- x) -> withObject "object" (parseProperties gProps x) obj
    Array gElements ->
      \(arr :- x) -> do
        (y, arr') <- withArray "array" (parseElements gElements x) arr
        unless (V.null arr') $ typeMismatch "end of array" (V.head arr')
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
parseElements (Element g) x arr =
  if V.null arr
    then fail "expected at least one more array element"
    else do
      y <- parseValue g (V.last arr :- x)
      return (y, V.init arr)
