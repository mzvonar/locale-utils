module Processor.Flatten where

import Prelude

import Data.Array (concatMap)
import Data.Locale (LocaleMap, Namespace(..), TranslationKey, TranslationValue(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (over)

flatten :: LocaleMap Namespace -> LocaleMap Namespace
flatten = map flattenNamespace
  where
    flattenNamespace :: Namespace -> Namespace
    flattenNamespace = over Namespace $ concatMap (flattenTranslation Nothing)

    flattenTranslation :: Maybe TranslationKey -> TranslationValue -> Array TranslationValue
    flattenTranslation parentKey (TranslationValue k v) = [TranslationValue (parentKey `concat` k) v]
    flattenTranslation parentKey (TranslationParent key translations) = concatMap (flattenTranslation (Just $ parentKey `concat` key)) translations

    concat :: Maybe TranslationKey -> TranslationKey -> TranslationKey
    concat Nothing k2 = k2
    concat (Just k1) k2 = k1 <> "." <> k2