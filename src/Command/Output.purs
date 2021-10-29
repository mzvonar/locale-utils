module Command.Output where
  
import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff (Aff, throwError, runAff_)
import Effect.Class (liftEffect)
import Node.Path (concat, isAbsolute, normalize)
import Node.Process (cwd)
import Options.Applicative (Mod, OptionFields, Parser, execParser, flag, fullDesc, help, helper, info, long, metavar, progDesc, short, showDefault, strArgument, strOption, value, (<**>))
import Processor.Output (Output(..))
import Processor.Output as P
import Processor.ReadInput as I

data Opts = Opts 
  { outputDir :: String
  , pipe :: Boolean
  }

type Path = String

outputOption :: Maybe (Mod OptionFields String) -> Parser Output
outputOption m = stdOutput <|> dirOutput
  where
    dirOutput :: Parser Output
    dirOutput = DirOutput <$> (strOption $ fold
                  [ long "output-dir"
                    , short 'o'
                    , metavar "outputDir"
                    , help "Directory to output the data to. Will contain subfolders with locale and each locale will contain namespace files with translations"
                  ] <> fromMaybe mempty m)

    stdOutput :: Parser Output
    stdOutput = pure StdOutput

outputOption' :: Path -> Parser Output
outputOption' p = outputOption $ Just $ value p <> showDefault

outputOption_ :: Parser Output
outputOption_ = outputOption Nothing

outputDirArgument :: Parser String
outputDirArgument = strArgument $ fold
                      [ metavar "output_dir"
                      , help "Directory to output results"
                      ]

pipeOption :: Parser Boolean
pipeOption = flag false true $ fold
              [ long "pipe"
              , short 'p'
              , help "If this flag is set it also pipes the output to next command"
              ]

outputOpts :: Parser Opts
outputOpts = ado
  outputDir <- outputDirArgument
  pipe <- pipeOption
  in Opts { outputDir, pipe }

save :: Aff Unit
save = do
  (Opts o) <- liftEffect (fixPath =<< execParser opts)
  value <- I.readInput I.StdInput
  P.output (DirOutput o.outputDir) value
  when o.pipe $ P.output StdOutput value
  where
    opts = info (outputOpts <**> helper)
      ( fullDesc
     <> progDesc "Saves namespace files and locale subdirectries into output directory"
      )

    fixPath :: Opts -> Effect Opts
    fixPath (Opts o) = do
      baseDir <- cwd
      pure $ if isAbsolute o.outputDir 
        then Opts o 
        else Opts (o {outputDir = normalize $ concat [baseDir, o.outputDir]})

main :: Effect Unit
main = runAff_ (\res -> do
  case res of
    Left e -> throwError e
    _ -> pure unit
  ) save
