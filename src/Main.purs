module Main where

import Prelude

-- import Data.Argonaut (encodeJson, stringifyWithIndent)
import Data.Either (Either)
import Data.Locale (Locale(..), LocaleMap(..), Namespace(..), TranslationValue(..))
import Data.Map (singleton)
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

namespace :: Namespace
namespace = Namespace
  [ TranslationValue "label" "Popis"
  , TranslationValue "hint" "Napis sem"
  ]

locale :: Locale Namespace
locale = Locale $ singleton "form.json" namespace

localeMap :: LocaleMap Namespace
localeMap = LocaleMap $ singleton "sk" locale

mappedId :: LocaleMap Namespace
mappedId = map identity localeMap



flatNamespace :: Namespace
flatNamespace = Namespace 
  [ 
    -- TranslationValue "input.password.label" "Heslo"
  -- , TranslationValue "input.password.hint" "Musi mat 8 znakov"
  TranslationValue "input.password" "Password"
  , TranslationValue "input.email" "Email"
  ]

flatLocale :: Locale Namespace
flatLocale = Locale $ singleton "form.json" flatNamespace

flatLocaleMap :: LocaleMap Namespace
flatLocaleMap = LocaleMap $ singleton "sk" flatLocale

unflattenedLocale :: Either String (LocaleMap Namespace)
unflattenedLocale = unflatten flatLocaleMap

-- main :: Effect Unit
-- main = launchAff_ do
--   _ <- W.writeDir $ W.Opts { outputDir: "./locale-output", data: localeMap }
--   pure unit

main :: Effect Unit
main = launchAff_ do
  opts <- readInput $ StdInput
  logShow opts
