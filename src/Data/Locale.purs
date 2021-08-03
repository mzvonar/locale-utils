module Data.Locale where
  
import Prelude

import Data.Argonaut (Json, caseJson, caseJsonObject, decodeJson, encodeJson, jsonEmptyObject, jsonParser, parseJson, stringify)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Combinators ((:=), (~>))
import Data.Array ((:))
import Data.Either (Either(..), either)
import Data.Foldable (class Foldable, foldr)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable, sequence)
import Data.Tuple (Tuple)
import Foreign.Object (Object)
import Foreign.Object as FO


class (EncodeJson a, DecodeJson a) <= NamespaceClass a
  

type TranslationKey = String

data TranslationValue = TranslationValue TranslationKey String

data NestedTranslationValue
  = NestedTranslationValue TranslationKey String
  | TranslationParent TranslationKey (Array NestedTranslationValue)

-- data Translation = Translation (Array NestedTranslationValue)

type NamespaceName = String
newtype Namespace = Namespace (Array TranslationValue)
newtype NestedNamespace = NestedNamespace (Array NestedTranslationValue)

newtype Locale a = Locale (Map NamespaceName a)

type LocaleName = String
newtype LocaleMap a = LocaleMap (Map LocaleName (Locale a))



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