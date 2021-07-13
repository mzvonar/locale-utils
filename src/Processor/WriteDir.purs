module Processor.WriteDir where

import Prelude

import Data.Argonaut (encodeJson, stringifyWithIndent)
import Data.Either (Either(..))
import Data.Locale (Locale(..), LocaleMap(..), LocaleName, Namespace, NamespaceName)
import Data.TraversableWithIndex (traverseWithIndex)
import Effect.Aff (Aff, throwError, runAff_)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (writeTextFile, mkdir)
import Node.FS.Aff.Mkdirp (mkdirp)
import Node.Path (concat)


data Opts = Opts 
  { outputDir :: String 
  , data :: LocaleMap
  }

writeDir :: Opts -> Aff Unit
writeDir (Opts { outputDir, data: (LocaleMap localeMap)}) = 
  void $ traverseWithIndex writeLocale localeMap

  where
    writeLocale :: LocaleName -> Locale -> Aff Unit
    writeLocale localeName (Locale locale) = void $ traverseWithIndex (writeNamespace localeName) locale

    writeNamespace :: LocaleName -> NamespaceName -> Namespace -> Aff Unit
    writeNamespace locale name namespace = do
      let 
        outputDir' = concat [outputDir, locale]
        outputPath = concat [outputDir', name]
      _ <- mkdirp outputDir'
      writeTextFile UTF8 outputPath (stringifyWithIndent 4 $ encodeJson namespace)