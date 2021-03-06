module Command.Validate.Free where

import Effect
import Prelude

import Ansi.Codes as Codes
import Ansi.Output as Ansi
import Control.Monad.Free (Free, liftF, foldFree)
import Control.Monad.State (StateT, gets, modify_, runStateT)
import Control.Monad.Writer (WriterT, execWriterT, tell)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.List.NonEmpty (NonEmptyList)
import Data.Locale (TranslationKey)
import Data.Locale.Translation (key)
import Data.Locale.Translation.Validated (InvalidTranslation(..), TranslationError(..), ValidTranslation(..), ValidatedLocale(..), ValidatedLocaleMap(..), ValidatedNamespace(..), ValidatedTranslation, VariableType(..))
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Data.Tuple (Tuple(..))
import Effect.Console as Console

data MessageType = Info | Error
type Filter = Maybe MessageType

derive instance Eq MessageType

data ResultF a
  = SetFilter Filter a
  | GetFilter (Filter -> a)
  | Result ValidatedLocaleMap a
  | LocaleResult ValidatedLocale a
  | NamespaceResult ValidatedNamespace a
  | TranslationResult ValidatedTranslation a
  | TranslationErrorResult (Tuple TranslationKey TranslationError) a

derive instance Functor ResultF

type Result = Free ResultF


setFilter :: Filter -> Result Unit
setFilter a = liftF $ SetFilter a unit

getFilter :: Result Filter
getFilter = liftF $ GetFilter identity

result :: ValidatedLocaleMap -> Result Unit
result a = liftF $ Result a unit

localeResult :: ValidatedLocale -> Result Unit
localeResult a = liftF $ LocaleResult a unit

namespaceResult :: ValidatedNamespace -> Result Unit
namespaceResult a = liftF $ NamespaceResult a unit

translationResult :: ValidatedTranslation -> Result Unit
translationResult a = liftF $ TranslationResult a unit

translationErrorResult :: TranslationKey -> TranslationError -> Result Unit
translationErrorResult k a = liftF $ TranslationErrorResult (Tuple k a) unit

type Level = Int
type State = 
  { level :: Level
  , filter :: Filter
  }
type Output a = StateT State (WriterT String Effect) a

renderToConsole :: Result Unit -> Effect Unit
renderToConsole r = do
  string <- execWriterT $ runStateT (interpret r) { level: 0, filter: Nothing }
  Console.log string

interpret :: Result ~> Output
interpret = foldFree go
  where
  go :: ResultF ~> Output
  go = case _ of
    SetFilter a next -> do
      modify_ \s -> s { filter = a }
      pure next      
    GetFilter next -> do
      filter <- getFilter'
      pure $ next filter
    Result a next -> do
      renderResult a 
      pure next
    LocaleResult a next -> do
      renderLocaleResult a 
      pure next
    NamespaceResult a next -> do
      renderNamespaceResult a 
      pure next
    TranslationResult a next -> do
      renderTranslationResult a 
      pure next
    TranslationErrorResult (Tuple k a) next -> do
      renderTranslationErrorResult k a 
      pure next
    

  getFilter' :: Output Filter
  getFilter' = gets _.filter
    

  renderResult :: ValidatedLocaleMap -> Output Unit
  renderResult (ValidLocaleMap _) = output' Info "Locales are valid"
  renderResult (InvalidLocales lm) = do
    output' Error "Problems found:"
    indent
    foldMap renderLocaleResult lm

  renderLocaleResult :: ValidatedLocale -> Output Unit
  renderLocaleResult (ValidLocale k _) = output Info $ "Locale " <> k <> " is valid"
  renderLocaleResult (ExtraneousLocale k) = output Error $ "Extraneous locale " <> variable k
  renderLocaleResult (MissingLocale k) = output Error $ "Missing locale " <> variable k
  renderLocaleResult (InvalidNamespaces k ns) = do
    output Error $ "Locale " <> variable k <> error " has invalid namespaces:"
    indent 
    foldMap renderNamespaceResult ns
    unindent

  renderNamespaceResult :: ValidatedNamespace -> Output Unit
  renderNamespaceResult (ValidNamespace k _) = output Info $ "Namespace " <> k <> " is valid"
  renderNamespaceResult (ExtraneousNamespace k) = output Error $ "Extraneous namespace " <> variable k
  renderNamespaceResult (MissingNamespace k) = output Error $ "Missing namespace " <> variable k
  renderNamespaceResult (UnknownNamespaceError k e) = output Error $ "Namespace " <> variable k <> error " error: " <> e
  renderNamespaceResult (InvalidTranslations k ts) = do
    output Error $ "Namespace " <> variable k <> error " has invalid translations:"
    indent
    foldMap renderTranslationResult ts
    unindent

  renderTranslationResult :: ValidatedTranslation -> Output Unit
  renderTranslationResult (Right (ValidTranslation t)) = output Info $ "Translation " <> key t <> " is valid"
  renderTranslationResult (Left (InvalidTranslation t { errors })) = foldMap (renderTranslationErrorResult $ key t) errors


  renderTranslationErrorResult :: TranslationKey -> TranslationError -> Output Unit
  renderTranslationErrorResult key = output Error <<< errorToString key
    where
      errorToString :: TranslationKey -> TranslationError -> String
      errorToString k (MissingVariable t var) = "Key " <> variable k <> error " is missing" <> variable (varType t) <> error "variable " <> variable var
      errorToString k (ExtraneousVariable t var) = "Key " <> variable k <> error " has extraneous" <> variable (varType t) <> error "variable " <> variable var
      errorToString _ (MissingKey k) = "Key " <> variable k <> error " is missing"
      errorToString _ (ExtraneousKey k) = "Key " <> variable k <> error " is extraneous"
      errorToString k (UnknownError e) = "Unknown error for key " <> variable k <> error ": " <> e

      varType :: VariableType -> String
      varType SingularVariable = " singular "
      varType PluralVariable = " plural "
      varType OnlySingularVariable = " "

  indent :: Output Unit
  indent = modify_ \s -> s { level = s.level + 1}

  unindent :: Output Unit
  unindent = modify_ \s -> s { level = max (s.level - 1) 0}

  output :: MessageType -> String -> Output Unit
  output t str = do
    filter <- getFilter'
    if shouldOutput filter t
      then output' t str
      else pure unit

  output' :: MessageType -> String -> Output Unit
  output' t str = do
    level <- getLevel
    tell $ (power "  " level) <> withTypeColor t str <> "\n"

  getLevel :: Output Level
  getLevel = gets _.level

  info :: String -> String
  info = withTypeColor Info

  error :: String -> String
  error = withTypeColor Error

  withTypeColor :: MessageType -> String -> String
  withTypeColor t s =
    let 
      color = case t of
        Info -> Codes.Green
        Error -> Codes.BrightRed 
    in Ansi.withGraphics (Ansi.foreground color) s

  variable :: String -> String
  variable = withGraphics' (Ansi.foreground Codes.Yellow <> Ansi.bold)

  shouldOutput :: Filter -> MessageType -> Boolean
  shouldOutput Nothing _ = true
  shouldOutput (Just f) t = f == t

  withGraphics' :: NonEmptyList Codes.GraphicsParam -> String -> String
  withGraphics' params text =
    Codes.escapeCodeToString (Codes.Graphics params) <>
    text


showResults :: Filter -> ValidatedLocaleMap -> Result Unit
showResults f lm = do
  setFilter f
  result lm
