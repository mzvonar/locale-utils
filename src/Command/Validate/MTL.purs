module Command.Validate.MTL where

import Prelude

import Control.Monad.Reader (Reader, ask, local, runReader)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Locale (TranslationKey)
import Data.Locale.Translation (key)
import Data.Locale.Translation.Validated (InvalidTranslation(..), TranslationError(..), ValidTranslation(..), ValidatedLocale(..), ValidatedLocaleMap(..), ValidatedNamespace(..), ValidatedTranslation, VariableType(..))
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Data.String (joinWith)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Console as Console


type Level = Int
data MessageType = Info | Error
type Filter = Maybe MessageType
type Env = 
  { level :: Level
  , filter :: Filter
  }
type Output = (Reader Env) String

derive instance Eq MessageType

logResults :: Filter -> ValidatedLocaleMap -> Effect Unit
logResults f lm = Console.log $ renderResults f lm

renderResults :: Filter -> ValidatedLocaleMap -> String
renderResults filter lm = runReader (results lm) { level: 0, filter }

results :: ValidatedLocaleMap -> Output
results (ValidLocaleMap _) = output' Info "Locales are valid"
results (InvalidLocales lm) = join 
  [ output' Error "Problems found:"
  , indent $ foldMap locale lm
  ]

locale :: ValidatedLocale -> Output
locale (ValidLocale k _) = output Info $ "Locale " <> k <> " is valid"
locale (ExtraneousLocale k) = output Error $ "Extraneous locale " <> k
locale (MissingLocale k) = output Error $ "Missing locale " <> k
locale (InvalidNamespaces k ns) = join
  [ output Error $ "Locale " <> k <> " has invalid namespaces:"
  , indent $ foldMap namespace ns
  ]

namespace :: ValidatedNamespace -> Output
namespace (ValidNamespace k _) = output Info $ "Namespace " <> k <> " is valid"
namespace (ExtraneousNamespace k) = output Error $ "Extraneous namespace " <> k
namespace (MissingNamespace k) = output Error $ "Missing namespace " <> k
namespace (UnknownNamespaceError k e) = output Error $ "Namespace " <> k <> " error: " <> e
namespace (InvalidTranslations k ts) = join
  [ output Error $ "Namespace " <> k <> " has invalid translations:"
  , indent $ foldMap translation ts
  ]

translation :: ValidatedTranslation -> Output
translation (Right (ValidTranslation t)) = output Info $ "Translation " <> key t <> " is valid"
translation (Left (InvalidTranslation t { errors })) = join $ map (translationError $ key t) errors

translationError :: TranslationKey -> TranslationError -> Output
translationError key = output Error <<< errorToString key
  where
    errorToString :: TranslationKey -> TranslationError -> String
    errorToString k (MissingVariable t var) = "Key " <> k <> " is missing" <> varType t <> "variable " <> var
    errorToString k (ExtraneousVariable t var) = "Key " <> k <> " has extraneous" <> varType t <> "variable " <> var
    errorToString _ (MissingKey k) = "Key " <> k <> " is missing"
    errorToString _ (ExtraneousKey k) = "Key " <> k <> " is extraneous"
    errorToString k (UnknownError e) = "Unknown error for key " <> k <> ": " <> e

    varType :: VariableType -> String
    varType SingularVariable = " singular "
    varType PluralVariable = " plural "
    varType OnlySingularVariable = " "

output :: MessageType -> String -> Output
output t str = do
  {level, filter} <- ask

  pure $ if shouldOutput t filter
    then (power "  " level) <> str
    else ""

output' :: MessageType -> String -> Output
output' _ str = do
  {level} <- ask
  pure $ (power "  " level) <> str

shouldOutput :: MessageType -> Filter -> Boolean
shouldOutput _ Nothing = true
shouldOutput t (Just filter) = t == filter

join :: Array Output -> Output
join = sequence >=> joinWith "\n" >>> pure

indent :: Output -> Output
indent = local $ \env@{level} -> env { level = level + 1}
