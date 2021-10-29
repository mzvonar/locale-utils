module Client.Capability.API where

import Prelude

import Api as Api
import Data.Either (Either)
import Data.Locale (LocaleMap, NestedNamespace)
import Halogen (HalogenM, lift)
import Payload.Client (mkClient)
import Payload.Client.ClientApi (class ClientApi)

type Error = String

-- class (Monad m, ClientApi routesSpec client) <= API m routesSpec client | routesSpec -> client where
--   client :: client

-- instance API m => API (HalogenM st act slots msg m) where
--   client = lift <<< client
--   updateLocaleMap = lift <<< updateLocaleMap