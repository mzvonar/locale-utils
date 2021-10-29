module Command.Flatten where
  
import Prelude

import Command.Output (outputOption')
import Command.ReadInput (inputOption)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, runAff_, throwError)
import Effect.Class (liftEffect)
import Options.Applicative (Parser, execParser, fullDesc, helper, info, progDesc, (<**>))
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
  output <- outputOption' "./localeProcess/flatenned"
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
    Left e -> throwError e
    _ -> pure unit
  ) flatten