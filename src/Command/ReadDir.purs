module Command.ReadDir where

import Prelude

import Data.Argonaut (encodeJson, stringify)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Locale (LocaleMap)
import Effect (Effect)
import Effect.Aff (Aff, throwError, runAff_)
import Effect.Console (log)
import Effect.Class (liftEffect)
import Node.Path (concat, isAbsolute, normalize)
import Node.Process (cwd)
import Options.Applicative (Parser, (<**>), helper, fullDesc, progDesc, info, strOption, long, short, metavar, help, execParser, showDefault, value)
import Processor.ReadDir (Opts(..))
import Processor.ReadDir (readDir) as P


readDirOpts :: Parser Opts
readDirOpts = ado
  inputDir <- strOption $ fold
                [ long "input-dir"
                , short 'i'
                , metavar "STRING"
                , help "Locale directory to read"
                , showDefault
                , value "./locale"
                ]
  in Opts { inputDir }

readDir :: Aff LocaleMap
readDir = P.readDir =<< liftEffect (fixPath =<< execParser opts)
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
  ) readDir

