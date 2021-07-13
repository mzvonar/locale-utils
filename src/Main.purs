module Main where

import Prelude

import Data.Argonaut (encodeJson, stringifyWithIndent)
import Data.Either (Either(..), either)
import Data.Locale (Locale(..), LocaleMap(..), Namespace(..), TranslationValue(..), strResult)
import Data.Map (singleton)
import Effect (Effect)
import Effect.Aff (Fiber(..), launchAff_, launchAff, throwError)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Effect.Exception (throw, error)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (writeTextFile)
import Processor.ReadDir (Opts(..), readDir)
import Processor.ReadInput (Input(..), readInput)

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

-- namespace :: Namespace
-- namespace = Namespace
--   [ TranslationValue "label" "Popis"
--   , TranslationValue "error" "Chyba"
--   ]

-- locale :: Locale
-- locale = Locale $ singleton "form.json" namespace

-- localeMap :: LocaleMap
-- localeMap = LocaleMap $ singleton "sk" locale


-- main :: Effect Unit
-- main = launchAff_ do
--   _ <- W.writeDir $ W.Opts { outputDir: "./locale-output", data: localeMap }
--   pure unit

main :: Effect Unit
main = launchAff_ do
  opts <- readInput $ StdInput
  logShow opts
