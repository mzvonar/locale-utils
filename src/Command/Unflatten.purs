module Command.Unflatten where
  
import Prelude

import Command.Output (outputOption)
import Command.ReadInput (inputOption)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, runAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Options.Applicative (Parser, execParser, fullDesc, helper, info, progDesc, (<**>))
import Processor.Flatten (flatten)
import Processor.Unflatten (unflatten) as P
import Processor.Output as O
import Processor.ReadInput as I


data Opts = Opts 
  { input :: I.Input
  , output :: O.Output
  }

unflattenOpts :: Parser Opts
unflattenOpts = ado
  input <- inputOption
  output <- outputOption
  in Opts { input, output }



unflatten :: Aff Unit
unflatten = do
  (Opts opts) <- liftEffect $ execParser parserOpts
  value <- I.readInput opts.input
  case P.unflatten $ flatten value of
    Right res -> do
      O.output opts.output res
    Left e -> throwError $ error e
  

  where
    parserOpts = info (unflattenOpts <**> helper)
      ( fullDesc
     <> progDesc "Unflattens flattened translations"
      )

main :: Effect Unit
main = runAff_ (\res -> do
  case res of
    Left e -> throwError e
    _ -> pure unit
  ) unflatten