{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}

module TypeScript where

import Grammar

import Control.Applicative ((<$>), (<*>))
import Control.Monad (unless)
import Control.Monad.State (State, execState, gets, modify)
import Data.Aeson (Value)
import qualified Data.Aeson as Ae
import qualified Data.HashMap.Strict as H
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import qualified Data.Text as T
import Language.TypeScript


toType :: GrammarMap -> Grammar Val t1 t2 -> Maybe Type
toType gm = go
  where
    go :: Grammar Val t1 t2 -> Maybe Type
    go = \case
      Id -> Nothing
      g1 :. g2 ->
        -- Produce the leftmost grammar
        case go g1 of
          Just ty -> Just ty
          Nothing -> go g2

      Empty -> Nothing
      g1 :<> g2 -> unify <$> go g1 <*> go g2

      Pure _ _ -> Nothing
      Many g -> go g

      Literal v -> Just (valueType v)

      Label n _ -> Just (TypeReference (TypeRef (TypeName Nothing (T.unpack n)) Nothing))

      Object g ->
        let toSig (n, (opt, ty)) = (emptyComment,
              PropertySignature (T.unpack n) opt (Just ty))
        in Just (ObjectType (TypeBody (map toSig (H.toList (toProperties gm g)))))

      Array g -> ArrayType <$> toElementType gm g

      Coerce ty _ -> Just ty

emptyComment :: CommentPlaceholder
emptyComment = Left (0, 0)

toProperties :: GrammarMap -> Grammar Obj t1 t2 -> HashMap Text (Maybe Optional, Type)
toProperties gm = go
  where
    go :: Grammar Obj t1 t2 -> HashMap Text (Maybe Optional, Type)
    go = \case
      Id -> H.empty
      g1 :. g2 ->
        H.unionWith (combineTuples bothOptional unify) (go g1) (go g2)

      Empty -> H.empty  -- TODO This is not the proper unit element
      g1 :<> g2 ->
        let props1 = go g1
            props2 = go g2
            markAllOptional = fmap (\(_, ty) -> (Just Optional, ty))
         in markAllOptional (H.difference props1 props2)
              `H.union`
            H.intersectionWith (combineTuples eitherOptional unify) props1 props2
              `H.union`
            markAllOptional (H.difference props2 props1)

      Pure _ _ -> H.empty
      Many g -> go g

      Property n g -> maybe H.empty (\ty -> H.singleton n (Nothing, ty)) (toType gm g)

toElementType :: GrammarMap -> Grammar Arr t1 t2 -> Maybe Type
toElementType gm = go
  where
    go :: Grammar Arr t1 t2 -> Maybe Type
    go = \case
      Id -> Nothing
      g1 :. g2 -> unify <$> go g1 <*> go g2

      Empty -> Nothing
      g1 :<> g2 -> unify <$> go g1 <*> go g2

      Pure _ _ -> Nothing
      Many g -> go g

      Element g -> toType gm g


combineTuples :: (a1 -> a2 -> a3) -> (b1 -> b2 -> b3) ->
                    (a1, b1) -> (a2, b2) -> (a3, b3)
combineTuples f g (x1, y1) (x2, y2) = (f x1 x2, g y1 y2)

bothOptional :: Maybe Optional -> Maybe Optional -> Maybe Optional
bothOptional (Just Optional) (Just Optional) = Just Optional
bothOptional _ _ = Nothing

eitherOptional :: Maybe Optional -> Maybe Optional -> Maybe Optional
eitherOptional Nothing Nothing = Nothing
eitherOptional _ _ = Just Optional

unify :: Type -> Type -> Type
unify ty1 ty2 | areTypesEqual ty1 ty2 = ty1
unify _ _ = Predefined AnyType

areTypesEqual :: Type -> Type -> Bool
areTypesEqual (Predefined AnyType) (Predefined AnyType) = True
areTypesEqual (Predefined NumberType) (Predefined NumberType) = True
areTypesEqual (Predefined BooleanType) (Predefined BooleanType) = True
areTypesEqual (Predefined StringType) (Predefined StringType) = True
areTypesEqual (Predefined VoidType) (Predefined VoidType) = True
-- TODO
areTypesEqual _ _ = False

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

      Coerce _ g   -> buildGrammarMap g

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
    makeType (SomeGrammar g) = toType gm g
