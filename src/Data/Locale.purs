module Data.Locale where
  
import Prelude

import Data.Argonaut (Json, JsonDecodeError(..), caseJson, caseJsonObject, caseJsonString, decodeJson, encodeJson, fromObject, fromString, isObject, isString, jsonEmptyArray, jsonEmptyObject, jsonParser, parseJson, stringify, stringifyWithIndent, toObject, toString)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Combinators ((:=), (~>))
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Array ((:))
import Data.Either (Either(..), either)
import Data.Foldable (foldr)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as FO

type TranslationKey = String
data TranslationValue
  = TranslationValue TranslationKey String
  | TranslationParent TranslationKey (Array TranslationValue)

-- data Translation = Translation (Array TranslationValue)

newtype Namespace = Namespace (Array TranslationValue)

type NamespaceName = String
newtype Locale = Locale (Map NamespaceName Namespace)

type LocaleName = String
newtype LocaleMap = LocaleMap (Map LocaleName Locale)



derive instance genericTranslationValue :: Generic TranslationValue _

instance showTranslationValue :: Show TranslationValue where
  show a = genericShow a

-- derive instance genericTranslation :: Generic Translation _

-- instance showTranslation :: Show Translation where
--   show a = genericShow a




derive instance genericNamespace :: Generic Namespace _
instance showNamespace :: Show Namespace where
  show a = genericShow a


derive instance Newtype Locale _
derive instance Generic Locale _
derive newtype instance EncodeJson Locale
derive newtype instance DecodeJson Locale

instance showLocale :: Show Locale where
  show = genericShow

derive instance Newtype LocaleMap _
derive instance genericLocaleMap :: Generic LocaleMap _

instance showLocaleMap :: Show LocaleMap where
  show = genericShow

derive newtype instance EncodeJson LocaleMap
derive newtype instance DecodeJson LocaleMap


{- JSON encoding/decoding -}
instance decodeJsonNamespace :: DecodeJson Namespace where
  decodeJson json = toTranslation json


toTranslation :: Json -> Either JsonDecodeError Namespace
toTranslation = caseJsonObject (Left $ TypeMismatch "Translation root is not an object") parse
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
toTranslationValue k = caseJson (l "") (l "Boolean") (l "Number") (\v -> Right $ TranslationValue k v) (l "Array") \o -> toParent k o
  where
    l :: forall a. String -> a -> Either JsonDecodeError _
    l t = \_ -> Left $ AtKey k $ TypeMismatch $ "Unkown translation type " <> show t

toParent :: String -> FO.Object Json -> Either JsonDecodeError TranslationValue
toParent k o = do
  p <- sequence $ foldrWithIndex (\k v acc -> (toTranslationValue k v):acc) [] o
  pure $ TranslationParent k p


instance encodeJsonTranslation :: EncodeJson Namespace where
  encodeJson a = fromTranslation a

fromTranslation :: Namespace -> Json
fromTranslation (Namespace t) = foldr ((~>) <<< fromTranslationValue) jsonEmptyObject t

fromTranslationValue :: TranslationValue -> Tuple String Json
fromTranslationValue (TranslationValue k v) = k := v
fromTranslationValue (TranslationParent k vs) = k := (foldr ((~>) <<< fromTranslationValue) jsonEmptyObject vs)



jstring :: String
jstring = """
{ 
  "key": "value",
  "key2": "value2",
  "parent": {
    "subKey": "subValue"
  }
}
  """

json' :: Either String Json
json' = jsonParser jstring

decodeAsTranslation :: String -> Either JsonDecodeError Namespace
decodeAsTranslation str = do
    json <- parseJson str
    -- obj <- (decodeJson =<< parseJson str) :: Either JsonDecodeError Namespace
    decodeJson json

decoded = decodeAsTranslation jstring

result :: String
result = either (\l -> "LEFT: " <> show l) (\r -> "RIGHT: " <> show r) $ decoded

-- trans = Parent (Map "")

strResult = either (\l -> "LEFT: " <> show l) (\r -> stringify (encodeJson r)) decoded