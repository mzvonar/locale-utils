module Client.Capability.Resource.LocaleMap where

import Prelude

import Data.Either (Either)
import Data.Locale (LocaleMap, NestedNamespace, SourceType)
import Halogen (HalogenM, lift)

type Error = String


class Monad m <= ManageLocaleMap m where
  getLocaleMap :: SourceType -> m (Either Error (LocaleMap NestedNamespace))
  updateLocaleMap :: LocaleMap NestedNamespace -> m (Either Error (LocaleMap NestedNamespace))

instance ManageLocaleMap m => ManageLocaleMap (HalogenM st act slots msg m) where
  getLocaleMap = lift <<< getLocaleMap
  updateLocaleMap = lift <<< updateLocaleMap