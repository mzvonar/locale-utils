module Processor.Output where

import Prelude

import Data.Argonaut (encodeJson, stringify)
import Data.Locale (class NamespaceClass, LocaleMap, NestedNamespace)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.Path (FilePath)
import Processor.WriteDir (writeDir) as P

data Output
  = DirOutput FilePath
  | StdOutput

derive instance Generic Output _
instance Show Output where
  show = genericShow

output :: forall a. NamespaceClass a => Output -> LocaleMap a -> Aff Unit
output (DirOutput outputDir) localeMap = P.writeDir outputDir localeMap
output StdOutput localeMap = liftEffect $ log $ stringify $ encodeJson localeMap