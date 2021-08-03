module Processor.Validate where

import Data.Function (on)
import Data.Locale (LocaleMap, Namespace)
import Data.Locale.Translation (fromLocaleMap)
import Data.Locale.Translation.Validated (ValidatedLocaleMap, validateLocaleMap)

validate :: LocaleMap Namespace -> LocaleMap Namespace -> ValidatedLocaleMap
validate = validateLocaleMap `on` fromLocaleMap