module Command.Merge where
  
import Prelude

import Command.Output (outputOption')
import Command.ReadInput (inputOption)
import Data.Either (Either(..), either)
import Data.Foldable (fold)
import Data.Function (on)
import Data.Locale (isNestedLocaleMap)
import Effect (Effect)
import Effect.Aff (Aff, runAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Options.Applicative (Parser, execParser, fullDesc, help, helper, info, long, metavar, progDesc, short, value, showDefault, strOption, (<**>))
import Processor.Flatten (flatten)
import Processor.Merge (merge) as P
import Processor.Output as O
import Processor.ReadInput as I
import Processor.Unflatten (unflatten)

data Opts = Opts 
  { input :: I.Input 
  , source :: I.Input
  , output :: O.Output
  }

sourceOption :: Parser I.Input
sourceOption = 
  I.DirInput <$> (strOption $ fold
    [ long "source-dir"
    , short 's'
    , metavar "sourceDir"
    , help "Directory with source locales to use merge onto"
    , value "./localeProcess/extracted"
    , showDefault
    ]
  )

mergeOpts :: Parser Opts
mergeOpts = ado
  input <- inputOption
  source <- sourceOption
  output <- outputOption' "./localeProcess/merged"
  in Opts { input, source, output }


merge :: Aff Unit
merge = do
  (Opts opts) <- liftEffect $ execParser parserOpts
  source <- I.readInput opts.source
  target <- I.readInput opts.input
  let isNested = isNestedLocaleMap target
  let result = (P.merge `on` flatten) source target

  if isNested 
    then do
      result' <- either (throwError <<< error) pure (unflatten result)
      O.output opts.output result'
    else O.output opts.output result

  where
    parserOpts = info (mergeOpts <**> helper)
      ( fullDesc
     <> progDesc "Merges input locales over source locales"
      )    
    

main :: Effect Unit
main = runAff_ (\res -> do
  case res of
    Left e -> throwError e
    _ -> pure unit
  ) merge