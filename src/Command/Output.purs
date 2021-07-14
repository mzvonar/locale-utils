module Command.Output where
  
import Prelude

import Control.Alt ((<|>))
import Data.Foldable (fold)
import Options.Applicative (Parser, help, long, metavar, short, strOption)
import Processor.Output (Output(..))

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