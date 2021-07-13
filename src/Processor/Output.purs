module Processor.Output where

import Prelude

import Data.Argonaut (encodeJson, stringify)
import Data.Locale (LocaleMap)
import Effect.Aff (Aff)
import Effect.Console (log)
import Effect.Class (liftEffect)
import Node.Path (FilePath)
import Processor.WriteDir (Opts(..), writeDir) as P

data Output
  = DirOutput FilePath
  | StdOutput

output :: Output -> LocaleMap -> Aff Unit
output (DirOutput outputDir) localeMap = P.writeDir $ P.Opts { outputDir, data: localeMap }
output StdOutput localeMap = liftEffect $ log $ stringify $ encodeJson localeMap