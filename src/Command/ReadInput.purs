module Command.ReadInput where
  
import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Locale (LocaleMap, NestedNamespace)
import Effect (Effect)
import Effect.Aff (Aff, runAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Options.Applicative (Parser, execParser, fullDesc, help, helper, info, long, metavar, progDesc, short, strOption, (<**>))
import Processor.ReadInput (Input(..))
import Processor.ReadInput (readInput) as P

data Opts = Opts { input :: Input }

inputOption :: Parser Input
inputOption = stdInput <|> dirInput
  where
    dirInput :: Parser Input
    dirInput = DirInput <$> (strOption $ fold
                    [ long "input-dir"
                    , short 'i'
                    , metavar "inputDir"
                    , help "Locale directory to read"
                    ]
                  )

    stdInput :: Parser Input
    stdInput = pure StdInput
    

readInputOpts :: Parser Opts
readInputOpts = ado
  input <- inputOption
  in Opts { input }

readInput :: Aff (LocaleMap NestedNamespace)
readInput = do
  (Opts opts) <- liftEffect $ execParser parserOpts
  P.readInput opts.input

  where
    parserOpts = info (readInputOpts <**> helper)
      ( fullDesc
     <> progDesc "If input-dir option is provided it reads the provided directory. Otherwise it reads from stdin"
      )


main :: Effect Unit
main = runAff_ (\res -> do
  case res of
    Right res' -> logShow res'
    Left e -> throwError e
  ) readInput