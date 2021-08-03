module Processor.WriteDir where

import Prelude

import Data.Argonaut (encodeJson, stringifyWithIndent)
import Data.Locale (class NamespaceClass, Locale(..), LocaleMap(..), LocaleName, NamespaceName, NestedNamespace)
import Data.TraversableWithIndex (traverseWithIndex)
import Effect.Aff (Aff)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (writeTextFile)
import Node.FS.Aff.Mkdirp (mkdirp)
import Node.Path (concat)


writeDir :: forall a. NamespaceClass a => String -> LocaleMap a -> Aff Unit
writeDir outputDir (LocaleMap localeMap) = 
  void $ traverseWithIndex writeLocale localeMap

  where
    writeLocale :: forall a. NamespaceClass a => LocaleName -> Locale a -> Aff Unit
    writeLocale localeName (Locale locale) = void $ traverseWithIndex (writeNestedNamespace localeName) locale

    writeNestedNamespace :: forall a. NamespaceClass a => LocaleName -> NamespaceName -> a -> Aff Unit
    writeNestedNamespace locale name namespace = do
      let 
        outputDir' = concat [outputDir, locale]
        outputPath = concat [outputDir', name]
      _ <- mkdirp outputDir'
      writeTextFile UTF8 outputPath (stringifyWithIndent 4 $ encodeJson namespace)