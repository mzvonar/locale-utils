module Command.Output where
  
import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Locale (LocaleMap)
import Effect (Effect)
import Effect.Aff (Aff, runAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Options.Applicative (Parser, execParser, fullDesc, help, helper, info, long, metavar, progDesc, short, strOption, (<**>))
import Processor.Output (Output(..))
import Processor.Output (output) as P

data Opts = Opts { output :: Output }

outputOption :: Parser Output
outputOption = stdOutput <|> dirOutput
  where
    dirOutput :: Parser Output
    dirOutput = DirOutput <$> (strOption $ fold
                    [ long "output-dir"
                    , short 'o'
                    , metavar "STRING"
                    , help "Directory to output the data to. Will contain subfolders with locale and each locale will contain namespace files with translations"
                    ]
                  )

    stdOutput :: Parser Output
    stdOutput = pure StdOutput
    

outputOpts :: Parser Opts
outputOpts = ado
  output <- outputOption
  in Opts { output }