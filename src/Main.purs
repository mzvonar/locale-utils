module Main where

import Prelude

-- import Data.Argonaut (encodeJson, stringifyWithIndent)
import Data.Either (Either)
import Data.Locale (Locale(..), LocaleMap(..), Namespace(..), NestedNamespace(..), NestedTranslationValue(..), TranslationValue(..))
import Data.Locale.Translation (Translation(..), fromNamespace, fromTranslationValue)
import Data.Locale.Translation.Validated (ValidatedNamespace, validateNamespace)
import Data.Map (singleton)
import Data.Maybe(Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (logShow)
-- import Effect.Console (log)
-- import Effect.Exception (throw, error)
-- import Node.Encoding (Encoding(..))
-- import Node.FS.Aff (writeTextFile)
-- import Processor.ReadDir (Opts(..), readDir)
import Processor.ReadInput (Input(..), readInput)
import Processor.Unflatten (unflatten)

-- main :: Effect Unit
-- main = launchAff_ do
--   locales <- readDir
--   logShow locales

-- main :: Effect Unit
-- main = logShow strResult


-- main :: Effect Unit
-- main = launchAff_ do
--     eitherRes <- readDir $ Opts { inputDir: "./locale" }
--     case eitherRes of
--         Right res -> writeTextFile UTF8 "./output.json" $ stringifyWithIndent 4 $ encodeJson res
--         Left e -> throwError $ error e

namespace :: NestedNamespace
namespace = NestedNamespace
  [ NestedTranslationValue "label" "Popis"
  , NestedTranslationValue "hint" "Napis sem"
  ]

locale :: Locale NestedNamespace
locale = Locale $ singleton "form.json" namespace

localeMap :: LocaleMap NestedNamespace
localeMap = LocaleMap $ singleton "sk" locale

mappedId :: LocaleMap NestedNamespace
mappedId = map identity localeMap



flatNestedNamespace :: Namespace
flatNestedNamespace = Namespace 
  [ 
    -- NestedTranslationValue "input.password.label" "Heslo"
  -- , NestedTranslationValue "input.password.hint" "Musi mat 8 znakov"
    TranslationValue "input.password" "Password"
  , TranslationValue "input.email" "Email"
  ]

flatLocale :: Locale Namespace
flatLocale = Locale $ singleton "form.json" flatNestedNamespace

flatLocaleMap :: LocaleMap Namespace
flatLocaleMap = LocaleMap $ singleton "sk" flatLocale

unflattenedLocale :: Either String (LocaleMap NestedNamespace)
unflattenedLocale = unflatten flatLocaleMap

-- main :: Effect Unit
-- main = launchAff_ do
--   _ <- W.writeDir $ W.Opts { outputDir: "./locale-output", data: localeMap }
--   pure unit

tWithoutVarsSrc :: TranslationValue
tWithoutVarsSrc = TranslationValue "someKey" "I don't have any vars"
tWithoutVarsTar :: TranslationValue
tWithoutVarsTar = TranslationValue "someKey" "Nemam premenne"

validTWithVarsSrc :: TranslationValue
validTWithVarsSrc = TranslationValue "someKeyVars" "I have {{count}} vars and {{beers}} beers"
validTWithVarsTar :: TranslationValue
validTWithVarsTar = TranslationValue "someKeyVars" "Mam {{count}} premennych a {{beers}} piv"

invalidTWithVarsSrc :: TranslationValue
invalidTWithVarsSrc = TranslationValue "someKeyVars" "I have {{count}} vars and {{beers}} beers"
invalidTWithVarsTar :: TranslationValue
invalidTWithVarsTar = TranslationValue "someKeyVars" "Mam {{counts}} premennych"


transWithoutVarsSrc :: Translation
transWithoutVarsSrc = fromTranslationValue tWithoutVarsSrc
transWithoutVarsTar :: Translation
transWithoutVarsTar = fromTranslationValue tWithoutVarsTar

validTransWithVarsSrc :: Translation
validTransWithVarsSrc = fromTranslationValue validTWithVarsSrc
validTransWithVarsTar :: Translation
validTransWithVarsTar = fromTranslationValue validTWithVarsTar

invalidTransWithVarsSrc :: Translation
invalidTransWithVarsSrc = fromTranslationValue invalidTWithVarsSrc
invalidTransWithVarsTar :: Translation
invalidTransWithVarsTar = fromTranslationValue invalidTWithVarsTar

validNamespaceSrc :: Namespace
validNamespaceSrc = Namespace [tWithoutVarsSrc, validTWithVarsSrc]
validNamespaceTar :: Namespace
validNamespaceTar = Namespace [tWithoutVarsTar, validTWithVarsTar]

invalidNamespaceSrc :: Namespace
invalidNamespaceSrc = Namespace [tWithoutVarsSrc, invalidTWithVarsSrc]
invalidNamespaceTar :: Namespace
invalidNamespaceTar = Namespace [tWithoutVarsTar, invalidTWithVarsTar]

missingNamespaceSrc :: Namespace
missingNamespaceSrc = Namespace [tWithoutVarsSrc, validTWithVarsSrc]
missingNamespaceTar :: Namespace
missingNamespaceTar = Namespace [tWithoutVarsTar]

extraNamespaceSrc :: Namespace
extraNamespaceSrc = Namespace [tWithoutVarsSrc]
extraNamespaceTar :: Namespace
extraNamespaceTar = Namespace [tWithoutVarsTar, validTWithVarsTar]

validKeys :: ValidatedNamespace
validKeys = validateNamespace (Just $ fromNamespace "form" validNamespaceSrc) (Just $ fromNamespace "form" validNamespaceTar)

invalidKeys :: ValidatedNamespace
invalidKeys = validateNamespace (Just $ fromNamespace "form" invalidNamespaceSrc) (Just $ fromNamespace "form" invalidNamespaceTar)

missingKeys :: ValidatedNamespace
missingKeys = validateNamespace (Just $ fromNamespace "form" missingNamespaceSrc) (Just $ fromNamespace "form" missingNamespaceTar)

extraKeys :: ValidatedNamespace
extraKeys = validateNamespace (Just $ fromNamespace "form" extraNamespaceSrc) (Just $ fromNamespace "form" extraNamespaceTar)

missing :: ValidatedNamespace
missing = validateNamespace (Just $ fromNamespace "form" validNamespaceSrc) Nothing

extra :: ValidatedNamespace
extra = validateNamespace Nothing (Just $ fromNamespace "form" extraNamespaceTar)

main :: Effect Unit
main = launchAff_ do
  opts <- readInput $ StdInput
  logShow opts
