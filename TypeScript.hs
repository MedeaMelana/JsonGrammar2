{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}

module TypeScript where

import Grammar

import Control.Monad (unless)
import Control.Monad.State (State, execState, gets, modify)
import Data.Aeson (Value)
import qualified Data.Aeson as Ae
import qualified Data.HashMap.Strict as H
import Data.HashMap.Strict (HashMap)
import Data.Monoid (Monoid(..), First(..), (<>))
import Data.Text (Text)
import qualified Data.Text as T
import Language.TypeScript


toType :: GrammarMap -> Grammar Val t1 t2 -> First Type
toType gm = go
  where
    ok = First . Just

    go :: Grammar Val t1 t2 -> First Type
    go = \case
      Id -> mempty  -- TODO
      g1 :. g2 -> go g1 <> go g2

      Empty -> mempty
      g1 :<> g2 -> go g1 <> go g2

      Pure _ _ -> mempty
      Many g -> go g

      Literal v -> ok (valueType v)

      Label n _ -> ok (TypeReference (TypeRef (TypeName Nothing (T.unpack n)) Nothing))

      Object g ->
        let toSig (n, ty) = (emptyComment,
              PropertySignature (T.unpack n) Nothing (Just ty))
        in ok (ObjectType (TypeBody (map toSig (H.toList (toProperties gm g)))))

      --Array g -> 

emptyComment :: CommentPlaceholder
emptyComment = Left (0, 0)

toProperties :: GrammarMap -> Grammar Obj t1 t2 -> HashMap Text Type
toProperties gm = \case
  Id -> H.empty
  g1 :. g2 -> H.union (toProperties gm g1) (toProperties gm g2)

  Empty -> H.empty  -- TODO
  g1 :<> g2 -> H.union (toProperties gm g1) (toProperties gm g2)  -- TODO

  Pure _ _ -> H.empty
  Many g -> toProperties gm g

  Property n g -> maybe H.empty (H.singleton n) (getFirst (toType gm g))  -- TODO

valueType :: Value -> Type
valueType = \case
  Ae.Object _ -> Predefined AnyType  -- TODO
  Ae.Array _  -> Predefined AnyType  -- TODO
  Ae.String _ -> Predefined StringType
  Ae.Number _ -> Predefined NumberType
  Ae.Bool _   -> Predefined BooleanType
  Ae.Null     -> Predefined VoidType  -- TODO

type GrammarMap = HashMap Text (SomeGrammar Val)

grammarMap :: [SomeGrammar Val] -> GrammarMap
grammarMap gs =
    execState (mapM_ (\(SomeGrammar g) -> buildGrammarMap g) gs) H.empty
  where
    buildGrammarMap :: Grammar c t1 t2 -> State GrammarMap ()
    buildGrammarMap = \case
      Id        -> return ()
      g1 :. g2  -> buildGrammarMap g1 >> buildGrammarMap g2

      Empty     -> return ()
      g1 :<> g2 -> buildGrammarMap g1 >> buildGrammarMap g2

      Pure _ _  -> return ()
      Many g    -> buildGrammarMap g

      Literal _ -> return ()

      Label n g -> do
        b <- gets (H.member n)
        unless b $ do
          modify (H.insert n (SomeGrammar g))
          buildGrammarMap g

      Object g     -> buildGrammarMap g
      Property _ g -> buildGrammarMap g

      Array g      -> buildGrammarMap g
      Element g    -> buildGrammarMap g

data SomeGrammar c where
  SomeGrammar :: Grammar c t1 t2 -> SomeGrammar c

interfaces :: [SomeGrammar Val] -> [DeclarationElement]
interfaces gs = tys
  where
    gm = grammarMap gs
    tys = [ InterfaceDeclaration emptyComment Nothing interface
          | (n, makeType -> Just (ObjectType body)) <- H.toList gm
          , let interface = Interface emptyComment (T.unpack n) Nothing Nothing body
          ]
    makeType (SomeGrammar g) = getFirst (toType gm g)
