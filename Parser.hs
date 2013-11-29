{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Parser where

import HypeScript
import Util

import Control.Applicative ((<$>))
import Control.Monad ((>=>), unless)
import Data.Aeson (Object, Array, withObject, (.:), withArray)
import Data.Aeson.Types (Parser, typeMismatch)
import Data.Monoid ((<>))
import qualified Data.Vector as V

parseValue :: Grammar Val t1 t2 -> t1 -> Parser t2
parseValue = \case
  -- Any context
  Id -> return
  g1 :. g2 -> parseValue g2 >=> parseValue g1
  Empty -> \_ -> fail "empty grammar"
  g1 :<> g2 -> parseValue g1 <> parseValue g2
  Pure f _ -> f
  Many g -> manyM (parseValue g)

  -- Value context
  Literal val -> \(val' :- t) ->
    if val == val'
      then return t
      else typeMismatch "literal" val'
  Object gProps -> \(val :- x) ->
    withObject "object" (\obj -> parseProperties obj gProps x) val
  Array gElements ->
    \(val :- x) -> do
      (arr', y) <- withArray "array" (\arr -> parseElements gElements (arr, x)) val
      unless (V.null arr') $ typeMismatch "end of array" (V.head arr')
      return y

parseProperties :: Object -> Grammar Obj t1 t2 -> t1 -> Parser t2
parseProperties obj = \case
  -- Any context
  Id -> return
  g1 :. g2 -> parseProperties obj g2 >=> parseProperties obj g1
  Empty -> \_ -> fail "empty grammar"
  g1 :<> g2 -> parseProperties obj g1 <> parseProperties obj g2
  Pure f _ -> f
  Many g -> manyM (parseProperties obj g)

  -- Object context
  Property n gVal -> \x -> do
    val <- obj .: n
    parseValue gVal (val :- x)

parseElements :: Grammar Arr t1 t2 -> (Array, t1) -> Parser (Array, t2)
parseElements = \case
  -- Any context
  Id -> return
  g1 :. g2 -> parseElements g2 >=> parseElements g1
  Empty -> \_ -> fail "empty grammar"
  g1 :<> g2 -> parseElements g1 <> parseElements g2
  Pure f _ -> \(arr, x) -> (arr, ) <$> f x
  Many g -> manyM (parseElements g)

  -- Array context
  Element gEl -> \(arr, x) ->
    if V.null arr
      then fail "expected at least one more array element"
      else do
        y <- parseValue gEl (V.last arr :- x)
        return (V.init arr, y)
