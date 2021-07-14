module Data.Locale where
  
import Prelude

import Data.Argonaut (Json, caseJson, caseJsonObject, decodeJson, encodeJson, jsonEmptyObject, jsonParser, parseJson, stringify)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Combinators ((:=), (~>))
import Data.Array ((:))
import Data.Either (Either(..), either)
import Data.Foldable (class Foldable, foldMap, foldMapDefaultR, foldlDefault, foldr, foldl, foldrDefault)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable, sequence, traverseDefault)
import Data.Tuple (Tuple)
import Foreign.Object (Object)
import Foreign.Object as FO
import Math (e)

type TranslationKey = String

data TranslationValue
  = TranslationValue TranslationKey String
  | TranslationParent TranslationKey (Array TranslationValue)

-- data Translation = Translation (Array TranslationValue)

newtype Namespace = Namespace (Array TranslationValue)

type NamespaceName = String
newtype Locale a = Locale (Map NamespaceName a)

type LocaleName = String
newtype LocaleMap a = LocaleMap (Map LocaleName (Locale a))



derive instance genericTranslationValue :: Generic TranslationValue _

instance showTranslationValue :: Show TranslationValue where
  show a = genericShow a

-- derive instance genericTranslation :: Generic Translation _

-- instance showTranslation :: Show Translation where
--   show a = genericShow a



derive instance Newtype Namespace _
derive instance genericNamespace :: Generic Namespace _
instance showNamespace :: Show Namespace where
  show a = genericShow a


derive instance Newtype (Locale a) _
derive instance Generic (Locale a) _
derive newtype instance Functor Locale
derive newtype instance Foldable Locale
derive newtype instance Traversable Locale
-- instance Functor Locale where
--   map :: forall a b. (a -> b) -> Locale a -> Locale b
--   map f (Locale l) = map f l 

-- instance Foldable Locale where
--   foldMap f (Locale l) = foldMap f l
--   foldl = foldlDefault
--   foldr = foldrDefault

derive newtype instance EncodeJson (Locale Namespace)
derive newtype instance DecodeJson (Locale Namespace)

instance showLocale :: Show a => Show (Locale a) where
  show = genericShow

derive instance Newtype (LocaleMap a) _
derive instance genericLocaleMap :: Generic (LocaleMap a) _

instance showLocaleMap :: Show a => Show (LocaleMap a) where
  show = genericShow

instance Functor LocaleMap where
  map :: forall a b. (a -> b) -> LocaleMap a -> LocaleMap b
  map f (LocaleMap l) = LocaleMap $ map (map f) l 

-- instance Foldable LocaleMap where
--   -- foldMap :: forall a m. Monoid m => (a -> m) -> LocaleMap a -> m
--   -- foldMap f (LocaleMap lm) = foldMap f lm
--   -- foldl = foldlDefault
--   -- foldr = foldrDefault
--   foldr f b (LocaleMap lm) = foldr f b ?lm
--   foldl f b (LocaleMap lm) = foldl f b lm
--   foldMap = foldMapDefaultR

-- instance Traversable LocaleMap where
--   sequence :: forall a m. Applicative m => LocaleMap (m a) -> m (LocaleMap a)
--   sequence (LocaleMap lm) = do
--     let lm' = map sequence lm
    
--     pure ?lm'
--     -- case (sequence $ ) of
--     --   Left e -> Left e
--     --   Right lm' -> pure $ LocaleMap lm'

--   traverse = traverseDefault
    

derive newtype instance EncodeJson (LocaleMap Namespace)
derive newtype instance DecodeJson (LocaleMap Namespace)


{- JSON encoding/decoding -}
instance decodeJsonNamespace :: DecodeJson Namespace where
  decodeJson json = toTranslation json

instance encodeJsonTranslation :: EncodeJson Namespace where
  encodeJson a = fromTranslation a

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