module Command.Validate.FT where

-- implementation using Final Tagless style

import Prelude

import Ansi.Codes as Codes
import Ansi.Output as Ansi
import Control.Monad.Reader (ReaderT, runReaderT, ask, asks, local)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader)
import Control.Monad.Writer (class MonadTell, WriterT, execWriterT, tell)
import Control.Monad.Writer.Class (class MonadTell, class MonadWriter)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.List.NonEmpty (NonEmptyList)
import Data.Locale (TranslationKey)
import Data.Locale.Translation (key)
import Data.Locale.Translation.Validated (InvalidTranslation(..), TranslationError(..), ValidTranslation(..), ValidatedLocale(..), ValidatedLocaleMap(..), ValidatedNamespace(..), ValidatedTranslation, VariableType(..))
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console as Console


data MessageType = Info | Error
type Filter = Maybe MessageType

derive instance Eq MessageType

class (Monad m) <= MonadTranslationResult m a where
  setFilter :: Filter -> m a -> m a
  result :: ValidatedLocaleMap -> m a
  localeResult :: ValidatedLocale -> m a
  namespaceResult :: ValidatedNamespace -> m a
  translationResult :: ValidatedTranslation -> m a
  translationErrorResult :: TranslationKey -> TranslationError -> m a

type Level = Int
type Env = 
  { level :: Level
  , filter :: Filter
  }
newtype Output a = Output (ReaderT Env (WriterT String Effect) a)

derive newtype instance Monoid (Output Unit)
derive newtype instance Functor Output
derive newtype instance Apply Output
derive newtype instance Applicative Output
derive newtype instance Bind Output
derive newtype instance Monad Output
-- derive newtype instance MonadEffect Output
derive newtype instance MonadReader Env Output
derive newtype instance MonadAsk Env Output
derive newtype instance MonadWriter String Output
derive newtype instance MonadTell String Output

instance MonadTranslationResult Output Unit where
  setFilter filter = local (\e -> e { filter = filter })
  result = renderResult
  localeResult = renderLocaleResult
  namespaceResult = renderNamespaceResult
  translationResult = renderTranslationResult
  translationErrorResult = renderTranslationErrorResult

renderToConsole :: Output Unit -> Effect Unit
renderToConsole (Output p) = do
  string <- execWriterT $ runReaderT p { level: 0, filter: Nothing }
  Console.log string

renderResult :: ValidatedLocaleMap -> Output Unit
renderResult (ValidLocaleMap _) = output' Info "Locales are valid"
renderResult (InvalidLocales lm) = do
  output' Error "Problems found:"
  indent $ foldMap renderLocaleResult lm


renderLocaleResult :: ValidatedLocale -> Output Unit
renderLocaleResult (ValidLocale k _) = output Info $ "Locale " <> k <> " is valid"
renderLocaleResult (ExtraneousLocale k) = output Error $ "Extraneous locale " <> variable k
renderLocaleResult (MissingLocale k) = output Error $ "Missing locale " <> variable k
renderLocaleResult (InvalidNamespaces k ns) = do
  output Error $ "Locale " <> variable k <> error " has invalid namespaces:"
  indent $ foldMap renderNamespaceResult ns

renderNamespaceResult :: ValidatedNamespace -> Output Unit
renderNamespaceResult (ValidNamespace k _) = output Info $ "Namespace " <> k <> " is valid"
renderNamespaceResult (ExtraneousNamespace k) = output Error $ "Extraneous namespace " <> variable k
renderNamespaceResult (MissingNamespace k) = output Error $ "Missing namespace " <> variable k
renderNamespaceResult (UnknownNamespaceError k e) = output Error $ "Namespace " <> variable k <> error " error: " <> e
renderNamespaceResult (InvalidTranslations k ts) = do
  output Error $ "Namespace " <> variable k <> error " has invalid translations:"
  indent $ foldMap renderTranslationResult ts

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

variable :: String -> String
variable = withGraphics (Ansi.foreground Codes.Yellow <> Ansi.bold)

error :: String -> String
error = withTypeColor Error

withGraphics :: NonEmptyList Codes.GraphicsParam -> String -> String
withGraphics params text =
  Codes.escapeCodeToString (Codes.Graphics params) <>
  text

indent :: Output Unit -> Output Unit
indent = local \e -> e { level = e.level + 1}

getFilter :: Output Filter
getFilter = asks _.filter

getLevel :: Output Level
getLevel = asks _.level

output :: MessageType -> String -> Output Unit
output t str = do
  filter <- getFilter
  if shouldOutput filter t
    then output' t str
    else pure unit

output' :: MessageType -> String -> Output Unit
output' t str = do
  level <- getLevel
  tell $ (power "  " level) <> withTypeColor t str <> "\n"

shouldOutput :: Filter -> MessageType -> Boolean
shouldOutput Nothing _ = true
shouldOutput (Just f) t = f == t

withTypeColor :: MessageType -> String -> String
withTypeColor t s =
  let 
    color = case t of
      Info -> Codes.Green
      Error -> Codes.BrightRed 
  in Ansi.withGraphics (Ansi.foreground color) s


showResults :: Filter -> ValidatedLocaleMap -> Output Unit
showResults f lm = do
  setFilter f do
    result lm