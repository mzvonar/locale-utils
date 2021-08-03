module Processor.ReadDir where

import Prelude

import Data.Argonaut (JsonDecodeError, decodeJson, parseJson)
import Data.Array (filter)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Locale (LocaleMap(..), Locale(..), NestedNamespace)
import Data.Map (Map, fromFoldable)
import Data.Traversable (traverse, sequence)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, throwError)
import Effect.Exception (error)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, readdir)
import Node.Path (concat)

data Opts = Opts { inputDir :: String }

readDir :: Opts -> Aff (LocaleMap NestedNamespace)
readDir (Opts opts) = do
  res <- read readLocaleDir opts.inputDir
  case res of
    (Right res') -> pure $ LocaleMap res'
    (Left e) -> throwError $ error e

    where
      readLocaleDir :: String -> Aff (Either String (Locale NestedNamespace))
      readLocaleDir localePath = pure <<< map Locale =<< read readTranslationFile localePath

      read :: forall a. (String -> Aff (Either String a)) -> String -> Aff (Either String (Map String a))
      read f path = do
        paths <- filterFiles <$> readdir path
        arrMaybeRes <- traverse call paths
        pure $ do
          res <- sequence arrMaybeRes
          pure $ fromFoldable res

        where
          call :: String -> Aff (Either String (Tuple String a))
          call p = do 
            maybeRes <- f $ joinPath path p
            pure $ do
                res <- maybeRes
                pure $ Tuple p res

      joinPath :: String -> String -> String
      joinPath a b = concat [a, b]

      filterFiles :: Array String -> Array String
      filterFiles = filter $ notEq ".DS_Store"

      decodeAsTranslation :: String -> Either JsonDecodeError NestedNamespace
      decodeAsTranslation str = do
          json <- parseJson str
          decodeJson json

      readTranslationFile :: String -> Aff (Either String NestedNamespace)
      readTranslationFile path = lmap show <$> decodeAsTranslation <$> (readTextFile UTF8) path
