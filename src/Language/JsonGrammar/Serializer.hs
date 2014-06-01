{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Language.JsonGrammar.Serializer (serializeValue) where

import Language.JsonGrammar.Grammar
import Language.JsonGrammar.Util

import Control.Applicative ((<$>), (<|>))
import Control.Monad ((>=>))
import qualified Data.Aeson as Ae
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V


-- | Convert a 'Grammar' to a JSON serializer.
serializeValue :: Grammar Val t1 t2 -> t2 -> Maybe t1
serializeValue = \case
  Id          -> return
  g1 :. g2    -> serializeValue g1 >=> serializeValue g2

  Empty       -> fail "empty grammar"
  g1 :<> g2   -> \x -> serializeValue g1 x <|> serializeValue g2 x

  Pure _ f    -> f
  Many g      -> manyM (serializeValue g)

  Literal val -> return . (val :-)

  Label _ g   -> serializeValue g

  Object g    -> \x -> do
    (obj, y) <- serializeProperties g (H.empty, x)
    return (Ae.Object obj :- y)

  Array g     -> \x -> do
    (arr, y) <- serializeElements g (V.empty, x)
    return (Ae.Array arr :- y)

  Coerce _ g -> serializeValue g


serializeProperties ::
  Grammar Obj t1 t2 -> (Ae.Object, t2) -> Maybe (Ae.Object, t1)
serializeProperties = \case
  Id           -> return
  g1 :. g2     -> serializeProperties g1 >=> serializeProperties g2

  Empty        -> fail "empty grammar"
  g1 :<> g2    -> \objx ->
    serializeProperties g1 objx <|> serializeProperties g2 objx

  Pure _ f     -> \(obj, x) -> (obj, ) <$> f x
  Many g       -> manyM (serializeProperties g)

  Property n g -> \(obj, x) -> do
    val :- y <- serializeValue g x
    return (H.insert n val obj, y)


serializeElements :: Grammar Arr t1 t2 -> (Ae.Array, t2) -> Maybe (Ae.Array, t1)
serializeElements = \case
  Id        -> return
  g1 :. g2  -> serializeElements g1 >=> serializeElements g2

  Empty     -> fail "empty grammar"
  g1 :<> g2 -> \x -> serializeElements g1 x <|> serializeElements g2 x

  Pure _ f  -> \(arr, x) -> (arr, ) <$> f x
  Many g    -> manyM (serializeElements g)

  Element g -> \(arr, x) -> do
    val :- y <- serializeValue g x
    return (V.snoc arr val, y)
