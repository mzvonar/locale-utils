module Processor.Flatten where

import Prelude

import Data.Array (concatMap)
import Data.Locale (LocaleMap, NestedNamespace(..), Namespace(..), TranslationKey, NestedTranslationValue(..), TranslationValue(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (over)

flatten :: LocaleMap NestedNamespace -> LocaleMap Namespace
flatten = map flattenNestedNamespace
  where
    flattenNestedNamespace :: NestedNamespace -> Namespace
    flattenNestedNamespace (NestedNamespace nestedValues) = 
      let values = concatMap (flattenTranslation Nothing) nestedValues
      in Namespace values

    flattenTranslation :: Maybe TranslationKey -> NestedTranslationValue -> Array TranslationValue
    flattenTranslation parentKey (NestedTranslationValue k v) = [TranslationValue (parentKey `concat` k) v]
    flattenTranslation parentKey (TranslationParent key translations) = concatMap (flattenTranslation (Just $ parentKey `concat` key)) translations

    concat :: Maybe TranslationKey -> TranslationKey -> TranslationKey
    concat Nothing k2 = k2
    concat (Just k1) k2 = k1 <> "." <> k2