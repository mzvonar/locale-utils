module Command.Read where

import Prelude

import Data.Argonaut (encodeJson, stringify)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Locale (LocaleMap, NestedNamespace)
import Effect (Effect)
import Effect.Aff (Aff, throwError, runAff_)
import Effect.Console (log)
import Effect.Class (liftEffect)
import Node.Path (concat, isAbsolute, normalize)
import Node.Process (cwd)
import Options.Applicative (Parser, execParser, fullDesc, help, helper, info, metavar, progDesc, showDefault, strArgument, value, (<**>))
import Processor.ReadDir (Opts(..))
import Processor.ReadDir (readDir) as P


readDirOpts :: Parser Opts
readDirOpts = ado
  inputDir <- strArgument $ fold
                [ metavar "INPUT_DIR"
                , help "Locale directory to read"
                , showDefault
                , value "./locale"
                ]
  -- inputDir <- strOption $ fold
  --               [ long "input-dir"
  --               , short 'i'
  --               , metavar "STRING"
  --               , help "Locale directory to read"
  --               , showDefault
  --               , value "./locale"
  --               ]
  in Opts { inputDir }

read :: Aff (LocaleMap NestedNamespace)
read = P.readDir =<< liftEffect (fixPath =<< execParser opts)
  where
    opts = info (readDirOpts <**> helper)
      ( fullDesc
     <> progDesc "Reads directory with locale subdirectries which contains namespace files"
      )

    fixPath :: Opts -> Effect Opts
    fixPath (Opts o) = do
      baseDir <- cwd
      pure $ if isAbsolute o.inputDir 
        then Opts o 
        else Opts (o {inputDir = normalize $ concat [baseDir, o.inputDir]})

-- main :: Effect Unit
-- main = launchAff_ do
--     eitherRes <- readDir
--     case eitherRes of
--         Right res -> log $ stringify $ encodeJson res
--         Left e -> throwError $ error e

main :: Effect Unit
main = runAff_ (\res -> do
  case res of
    Right res'' -> log $ stringify $ encodeJson res''
    Left e -> throwError e
  ) read

