module Command.Validate where
  
import Prelude

import Command.Output (outputOption)
import Command.ReadInput (inputOption)
import Command.Validate.Free as Free
import Command.Validate.MTL as MTL
import Control.Monad.Reader (runReaderT)
import Data.Argonaut (stringify, encodeJson)
import Data.Array (null, catMaybes)
import Data.Either (Either(..))
import Data.Foldable (fold, foldMap)
import Data.Function (on)
import Data.Locale (TranslationKey)
import Data.Locale.Translation (key)
import Data.Locale.Translation.Validated (InvalidTranslation(..), TranslationError(..), ValidTranslation(..), ValidatedLocale(..), ValidatedLocaleMap(..), ValidatedNamespace(..), ValidatedTranslation, VariableType(..), isValidLocale)
import Data.Map (filter, values) as M
import Data.Maybe (Maybe(..))
import Data.String.Utils (unsafeRepeat)
import Data.Traversable (sequence, traverse, traverse_)
import Data.Validated (Validated(..))
import Debug (spy)
import Effect (Effect)
import Effect.Aff (Aff, runAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Options.Applicative (Parser, execParser, fullDesc, helper, info, progDesc, strOption, short, long, metavar, help, (<**>))
import Processor.Flatten (flatten)
import Processor.Output as O
import Processor.ReadInput as I
import Processor.Validate (validate) as P

data Opts = Opts 
  { input :: I.Input 
  , source :: I.Input
  }

data LogFilter
  = LogAll
  | LogValid
  | LogInvalid

sourceOption :: Parser I.Input
sourceOption = 
  I.DirInput <$> (strOption $ fold
    [ long "source-dir"
    , short 's'
    , metavar "STRING"
    , help "Directory with source locales to use as base for validation (usually extracted keys)"
    ]
  )

validateOpts :: Parser Opts
validateOpts = ado
  input <- inputOption
  source <- sourceOption
  in Opts { input, source }

data MessageType = Info | Error

data Message
  = Message MessageType String
  | NestedMessages (Array Message)

derive instance Eq MessageType

validate :: Aff Unit
validate = do
  (Opts opts) <- liftEffect $ execParser parserOpts
  source <- I.readInput opts.source
  target <- I.readInput opts.input
  let results = (P.validate `on` flatten) source target
  liftEffect $ useFree results

  where
    parserOpts = info (validateOpts <**> helper)
      ( fullDesc
     <> progDesc "Validates locales"
      )

    useMtl = MTL.logResults
    useFree = Free.renderToConsole <<< Free.showResults (Just Free.Error)

logResults :: ValidatedLocaleMap -> Effect Unit
logResults results =
  if null messages
    then log "Locales are valid"
    else do
      log "Problems found:"
      traverse_ (logMessage 1) messages
  where
    messages :: Array Message
    messages = filterMessages Error $ createMessages results

    filterMessages :: MessageType -> Array Message -> Array Message
    filterMessages t ms = catMaybes $ map (filterMessage t) ms

    filterMessage :: MessageType -> Message -> Maybe Message
    filterMessage t m@(Message mt _) | mt == t   = Just m
                                     | otherwise = Nothing
    filterMessage t (NestedMessages ms) =
        let filtered = catMaybes $ map (filterMessage t) ms
        in if null filtered
          then Nothing
          else Just $ NestedMessages filtered

    logMessage :: Int -> Message -> Effect Unit
    logMessage i (Message _ m) = log $ indent i <> m
      where
        indent :: Int -> String
        indent n = unsafeRepeat n "  "
    logMessage i (NestedMessages ms) = traverse_ (logMessage $ i + 1) ms

    createMessages :: ValidatedLocaleMap -> Array Message
    createMessages (ValidLocaleMap _) = []
    createMessages (InvalidLocales lm) = foldMap localeMessage lm
      where
        localeMessage :: ValidatedLocale -> Array Message
        localeMessage (ValidLocale k _) = [Message Info $ "Locale " <> k <> " is valid"]
        localeMessage (ExtraneousLocale k) = [Message Error $ "Extraneous locale " <> k]
        localeMessage (MissingLocale k) = [Message Error $ "Missing locale " <> k]
        localeMessage (InvalidNamespaces k ns) =
          [ Message Error $ "Locale " <> k <> " has invalid namespaces:"
          , NestedMessages $ foldMap namespaceMessage ns
          ]
        
        namespaceMessage :: ValidatedNamespace -> Array Message
        namespaceMessage (ValidNamespace k _) = [Message Info $ "Namespace " <> k <> " is valid"]
        namespaceMessage (ExtraneousNamespace k) = [Message Error $ "Extraneous namespace " <> k]
        namespaceMessage (MissingNamespace k) = [Message Error $ "Missing namespace " <> k]
        namespaceMessage (UnknownNamespaceError k e) = [Message Error $ "Namespace " <> k <> " error: " <> e]
        namespaceMessage (InvalidTranslations k ts) =
          [ Message Error $ "Namespace " <> k <> " has invalid translations:"
          , NestedMessages $ foldMap translationMessage ts
          ]

        translationMessage :: ValidatedTranslation -> Array Message
        translationMessage (Right (ValidTranslation t)) = [Message Info $ "Translation " <> key t <> " is valid"]
        translationMessage (Left (InvalidTranslation t { errors })) = map (translationErrorMessage $ key t) errors

        translationErrorMessage :: TranslationKey -> TranslationError -> Message
        translationErrorMessage k (MissingVariable t var) = Message Error $ "Key " <> k <> " is missing" <> varType t <> "variable " <> var
        translationErrorMessage k (ExtraneousVariable t var) = Message Error $ "Key " <> k <> " has extraneous" <> varType t <> "variable " <> var
        translationErrorMessage _ (MissingKey k) = Message Error $ "Key " <> k <> " is missing"
        translationErrorMessage _ (ExtraneousKey k) = Message Error $ "Key " <> k <> " is extraneous"
        translationErrorMessage k (UnknownError e) = Message Error $ "Unknown error for key " <> k <> ": " <> e

        varType :: VariableType -> String
        varType SingularVariable = " singular "
        varType PluralVariable = " plural "
        varType OnlySingularVariable = " "


renderLog :: ValidatedLocaleMap -> Effect Unit
renderLog (ValidLocaleMap _) = log "Locales are valid"
renderLog (InvalidLocales lm) = do
  log "Problems found:"
  traverse_ (logLocale LogInvalid) lm
    
  where
    logLocale :: LogFilter -> ValidatedLocale -> Effect Unit
    logLocale lf (ValidLocale k _) | logValid lf = log $ "Locale " <> k <> " is valid"
    logLocale lf (ExtraneousLocale k) | logInvalid lf = log $ "Extraneous locale " <> k
    logLocale lf (MissingLocale k) | logInvalid lf = log $ "Missing locale " <> k
    logLocale lf (InvalidNamespaces k ns) | logInvalid lf = do
      log $ "Locale " <> k <> " has invalid namespaces:\n"
      traverse_ (logNamespace lf) ns
    logLocale _ _ = pure unit

    logNamespace :: LogFilter -> ValidatedNamespace -> Effect Unit
    logNamespace lf (ValidNamespace k _) | logValid lf = log $ "Namespace " <> k <> " is valid"
    logNamespace lf (ExtraneousNamespace k) | logValid lf = log $ "Extraneous namespace " <> k
    logNamespace lf (MissingNamespace k) | logInvalid lf = log $ "Missing namespace " <> k
    logNamespace lf (InvalidTranslations k ts) | logInvalid lf = do
      log $ "Namespace " <> k <> " has invalid translations:\n"
      traverse_ (logTranslation lf) ts
    logNamespace _ _ = pure unit

    logTranslation :: LogFilter -> ValidatedTranslation -> Effect Unit
    logTranslation lf (Right (ValidTranslation t)) | logValid lf = log $ "Translation " <> key t <> " is valid"
    logTranslation lf (Left (InvalidTranslation t { errors })) | logInvalid lf = traverse_ (logTranslationError $ key t) errors
    logTranslation _ _ = pure unit

    logTranslationError :: TranslationKey -> TranslationError -> Effect Unit
    logTranslationError k (MissingVariable t var) = log $ "Key " <> k <> " is missing " <> varType t <> " variable " <> var
    logTranslationError k (ExtraneousVariable t var) = log $ "Key " <> k <> " has extraneous " <> varType t <> " variable " <> var
    logTranslationError _ (MissingKey k) = log $ "Key " <> k <> " is missing"
    logTranslationError _ (ExtraneousKey k) = log $ "Key " <> k <> " is extraneous"
    logTranslationError k (UnknownError e) = log $ "Unknown error for key " <> k <> ": " <> e

    varType :: VariableType -> String
    varType SingularVariable = "singular"
    varType PluralVariable = "plural"
    varType OnlySingularVariable = ""

    logValid :: LogFilter -> Boolean
    logValid LogInvalid = false
    logValid _ = true

    logInvalid :: LogFilter -> Boolean
    logInvalid LogValid = false
    logInvalid _ = true

    

main :: Effect Unit
main = runAff_ (\res -> do
  case res of
    Left e -> throwError e
    _ -> pure unit
  ) validate