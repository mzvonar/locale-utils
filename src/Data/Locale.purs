module Data.Locale where
  
import Prelude

import Ansi.Codes (RenderingMode(..))
import Data.Argonaut (Json, caseJson, caseJsonObject, decodeJson, encodeJson, jsonEmptyObject, jsonParser, parseJson, stringify)
import Data.Argonaut as A
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError(..), printJsonDecodeError)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Combinators ((:=), (~>))
import Data.Array ((:), (!!))
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Foldable (class Foldable, foldr)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.List (List(..))
import Data.List.NonEmpty as NonEmptyList
import Data.Map (Map, values)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable, sequence)
import Data.Tuple (Tuple)
import Debug as D
import Foreign (ForeignError(ForeignError))
import Foreign.Object (Object)
import Foreign.Object as FO
import Payload.Client.DecodeResponse (class DecodeResponse, unexpectedError, jsonDecodeError, unhandled) as PDR
import Payload.Client.EncodeParam (class EncodeParam)
import Payload.ContentType as ContentType
import Payload.Headers (Headers)
import Payload.Headers as Headers
import Payload.ResponseTypes (Response(..), ResponseBody(..))
import Payload.Server.Handlers (File)
import Payload.Server.Params (class DecodeParam, decodeParam)
import Payload.Server.Response (class EncodeResponse)
import Payload.TypeErrors (type (<>), type (|>))
import Prim.TypeError (class Warn, Quote, Text)

data SourceType = ExtractedSource | TranslatedSource

type TranslationKey = String

data TranslationValue = TranslationValue TranslationKey String

data NestedTranslationValue
  = NestedTranslationValue TranslationKey String
  | TranslationParent TranslationKey (Array NestedTranslationValue)

-- data Translation = Translation (Array NestedTranslationValue)

type NamespaceName = String
newtype Namespace = Namespace (Array TranslationValue)
newtype NestedNamespace = NestedNamespace (Array NestedTranslationValue)

class (EncodeJson a, DecodeJson a) <= NamespaceClass a

newtype Locale a = Locale (Map NamespaceName a)

type LocaleName = String
newtype LocaleMap a = LocaleMap (Map LocaleName (Locale a))

instance EncodeParam SourceType where
  encodeParam ExtractedSource = "source"
  encodeParam TranslatedSource = "translated"

instance DecodeParam SourceType where
  decodeParam "source" = Right ExtractedSource
  decodeParam "translated" = Right TranslatedSource
  decodeParam s = Left $ "Could not decode '" <> s <> "' into an SourceType"

derive instance Generic TranslationValue _
instance Show TranslationValue where
  show = genericShow

derive instance genericNestedTranslationValue :: Generic NestedTranslationValue _
instance showNestedTranslationValue :: Show NestedTranslationValue where
  show a = genericShow a


derive instance Newtype Namespace _
derive instance Generic Namespace _
instance Show Namespace where
  show = genericShow
instance NamespaceClass Namespace
instance DecodeJson Namespace where
  decodeJson json = toNamespace json
instance EncodeJson Namespace where
  encodeJson a = fromNamespace a

derive instance Newtype NestedNamespace _
derive instance genericNestedNamespace :: Generic NestedNamespace _
instance showNestedNamespace :: Show NestedNamespace where
  show a = genericShow a
instance NamespaceClass NestedNamespace
instance DecodeJson NestedNamespace where
  decodeJson json = toNestedNamespace json
instance EncodeJson NestedNamespace where
  encodeJson a = fromNestedNamespace a

derive instance Newtype (Locale a) _
derive instance Generic (Locale a) _
derive newtype instance Functor Locale
derive newtype instance FunctorWithIndex NamespaceName Locale
derive newtype instance Foldable Locale
derive newtype instance Traversable Locale

derive newtype instance DecodeJson (Locale Namespace)
derive newtype instance DecodeJson (Locale NestedNamespace)
instance NamespaceClass a => EncodeJson (Locale a) where
  encodeJson (Locale l) = encodeJson $ map encodeJson l

instance showLocale :: Show a => Show (Locale a) where
  show = genericShow

derive instance Newtype (LocaleMap a) _
derive instance genericLocaleMap :: Generic (LocaleMap a) _

instance showLocaleMap :: Show a => Show (LocaleMap a) where
  show = genericShow

instance Functor LocaleMap where
  map :: forall a b. (a -> b) -> LocaleMap a -> LocaleMap b
  map f (LocaleMap l) = LocaleMap $ map (map f) l 
instance FunctorWithIndex NamespaceName LocaleMap where
  mapWithIndex f (LocaleMap l) = LocaleMap $ map (mapWithIndex f) l

derive newtype instance DecodeJson (LocaleMap Namespace)
derive newtype instance DecodeJson (LocaleMap NestedNamespace)
instance NamespaceClass a => EncodeJson (LocaleMap a) where
  encodeJson (LocaleMap lm) = encodeJson $ map encodeJson lm
-- instance NamespaceClass a => DecodeJson (LocaleMap a) where
--   decodeJson json = decodeJson json

instance NamespaceClass a => EncodeResponse (LocaleMap a) where
  encodeResponse (Response r@{ body: json }) = pure $ Response $
        { status: r.status
        , headers: Headers.setIfNotDefined "content-type" ContentType.json r.headers
        , body: StringBody (stringify $ encodeJson json) }

instance PDR.DecodeResponse (LocaleMap Namespace) where
  decodeResponse (StringBody s) = lmap (PDR.jsonDecodeError <<< NonEmptyList.singleton <<< ForeignError <<< printJsonDecodeError) do
    json <- parseJson s
    decodeJson json
  decodeResponse b = PDR.unexpectedError "StringBody" b

instance PDR.DecodeResponse (LocaleMap NestedNamespace) where
  decodeResponse (StringBody s) = lmap (PDR.jsonDecodeError <<< NonEmptyList.singleton <<< ForeignError <<< printJsonDecodeError) do
    json <- parseJson s
    decodeJson json
  decodeResponse b = PDR.unexpectedError "StringBody" b

key :: TranslationValue -> TranslationKey
key (TranslationValue k _) = k

toNamespace :: Json -> Either JsonDecodeError Namespace
toNamespace = caseJsonObject (Left $ TypeMismatch "Translation root is not an object") parse
  where
    parse :: Object Json -> Either JsonDecodeError Namespace
    parse o = do
      t <- toArray o
      pure $ Namespace t
      -- pure $ case t of
      --   Right t -> Translation t
      --   Left e -> e

    toArray :: Object Json -> Either JsonDecodeError (Array TranslationValue)
    toArray o = sequence $ foldrWithIndex (\k v acc -> (toTranslationValue k v):acc) [] o

    toTranslationValue :: String -> Json -> Either JsonDecodeError TranslationValue
    toTranslationValue k = caseJson (l "") (l "Boolean") (l "Number") (\v -> Right $ TranslationValue k v) (l "Array") (l "Object")
      where
        l :: forall a. String -> a -> Either JsonDecodeError _
        l t = \_ -> Left $ AtKey k $ TypeMismatch $ "Unkown translation type " <> show t

fromNamespace :: Namespace -> Json
fromNamespace (Namespace t) = foldr ((~>) <<< translationValueToJson) jsonEmptyObject t

translationValueToJson :: TranslationValue -> Tuple String Json
translationValueToJson (TranslationValue k v) = k := v


toNestedNamespace :: Json -> Either JsonDecodeError NestedNamespace
toNestedNamespace = caseJsonObject (Left $ TypeMismatch "Translation root is not an object") parse
  where
    parse :: Object Json -> Either JsonDecodeError NestedNamespace
    parse o = do
      t <- toArray o
      pure $ NestedNamespace t
      -- pure $ case t of
      --   Right t -> Translation t
      --   Left e -> e

    toArray :: Object Json -> Either JsonDecodeError (Array NestedTranslationValue)
    toArray o = sequence $ foldrWithIndex (\k v acc -> (toNestedTranslationValue k v):acc) [] o

toNestedTranslationValue :: String -> Json -> Either JsonDecodeError NestedTranslationValue
toNestedTranslationValue k = caseJson (l "") (l "Boolean") (l "Number") (\v -> Right $ NestedTranslationValue k v) (l "Array") \o -> toParent k o
  where
    l :: forall a. String -> a -> Either JsonDecodeError _
    l t = \_ -> Left $ AtKey k $ TypeMismatch $ "Unkown translation type " <> show t

toParent :: String -> FO.Object Json -> Either JsonDecodeError NestedTranslationValue
toParent k o = do
  p <- sequence $ foldrWithIndex (\k v acc -> (toNestedTranslationValue k v):acc) [] o
  pure $ TranslationParent k p


fromNestedNamespace :: NestedNamespace -> Json
fromNestedNamespace (NestedNamespace t) = foldr ((~>) <<< fromNestedTranslationValue) jsonEmptyObject t

fromNestedTranslationValue :: NestedTranslationValue -> Tuple String Json
fromNestedTranslationValue (NestedTranslationValue k v) = k := v
fromNestedTranslationValue (TranslationParent k vs) = k := (foldr ((~>) <<< fromNestedTranslationValue) jsonEmptyObject vs)

isNestedLocaleMap :: LocaleMap NestedNamespace -> Boolean
isNestedLocaleMap (LocaleMap lm) = hasNestedLocale locales
  where
    locales = Map.values lm

    hasNestedLocale :: List (Locale NestedNamespace) -> Boolean
    hasNestedLocale Nil = false
    hasNestedLocale (Cons l ls) = case isNestedLocale l of
      true -> true
      false -> hasNestedLocale ls

    isNestedLocale :: Locale NestedNamespace -> Boolean
    isNestedLocale (Locale l) = go values
      where
        values = Map.values l

        go :: List NestedNamespace -> Boolean
        go Nil = false
        go (Cons n ns) = case isNestedNamespace n of
          true -> true
          false -> go ns

    isNestedNamespace :: NestedNamespace -> Boolean
    isNestedNamespace (NestedNamespace vs) = hasNestedValue vs

    hasNestedValue :: Array NestedTranslationValue -> Boolean
    hasNestedValue v = go 0 v
      where
        go :: Int -> Array NestedTranslationValue -> Boolean
        go _ [] = false
        go i vs = case vs !! i of
          Nothing -> false
          Just (TranslationParent _ _) -> true
          _ -> go (i + 1) vs