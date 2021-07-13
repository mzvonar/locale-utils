module Processor.Flatten where

import Prelude

import Data.Array (concatMap)
import Data.Locale (Locale(..), LocaleMap(..), Namespace(..), TranslationValue(..), TranslationKey)
import Data.Maybe (Maybe(..))

flatten :: LocaleMap -> LocaleMap
flatten (LocaleMap localeMap) = LocaleMap $ map flattenLocale localeMap
  where
    flattenLocale :: Locale -> Locale
    flattenLocale (Locale locale) = Locale $ map flattenNamespace locale

    flattenNamespace :: Namespace -> Namespace
    flattenNamespace (Namespace translations) = Namespace $ concatMap (flattenTranslation Nothing) translations

    flattenTranslation :: Maybe TranslationKey -> TranslationValue -> Array TranslationValue
    flattenTranslation parentKey (TranslationValue k v) = [TranslationValue (parentKey `concat` k) v]
    flattenTranslation parentKey (TranslationParent key translations) = concatMap (flattenTranslation (Just $ parentKey `concat` key)) translations

    concat :: Maybe TranslationKey -> TranslationKey -> TranslationKey
    concat Nothing k2 = k2
    concat (Just k1) k2 = k1 <> "." <> k2