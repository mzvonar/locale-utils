module Command.Flatten where
  
import Prelude

import Command.Output (outputOption)
import Command.ReadInput (inputOption)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Locale (LocaleMap)
import Effect (Effect)
import Effect.Aff (Aff, runAff_, throwError)
import Effect.Class (liftEffect)
import Options.Applicative (Parser, execParser, fullDesc, help, helper, info, long, metavar, progDesc, short, strOption, showDefault, value, (<**>))
import Processor.Flatten (flatten) as P
import Processor.Output as O
import Processor.ReadInput as I

data Opts = Opts 
  { input :: I.Input
  , output :: O.Output
  }

flattenOpts :: Parser Opts
flattenOpts = ado
  input <- inputOption
  output <- outputOption
  in Opts { input, output }



flatten :: Aff Unit
flatten = do
  (Opts opts) <- liftEffect $ execParser parserOpts
  value <- I.readInput opts.input
  O.output opts.output $ P.flatten value

  where
    parserOpts = info (flattenOpts <**> helper)
      ( fullDesc
     <> progDesc "Flattens the translations"
      )

main :: Effect Unit
main = runAff_ (\res -> do
  case res of
    Right res' -> pure unit
    Left e -> throwError e
  ) flatten