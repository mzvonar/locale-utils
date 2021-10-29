module Client.Api.Endpoint where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Maybe (Maybe(..))
import Routing.Duplex (RouteDuplex', int, optional, prefix, root, segment, string)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/), (?))

data Endpoint
  = LocaleMap

derive instance Generic Endpoint _

-- Our codec will cause a compile-time error if we fail to handle any of our
-- route cases.

-- | We need to be able to write our `Endpoint` type to a valid path in order to make requests. We
-- | can use `routing-duplex` the same way we did with our `Route` type to provide both a printer
-- | (write to `String`) and a parser (parse from `String`) that stays in sync with our `Endpoint`
-- | type automatically.
-- |
-- | For a full treatment of how this function produces both a parser and printer guaranteed to
-- | produce valid paths, see the `routing-duplex` tutorial:
-- | https://github.com/natefaubion/purescript-routing-duplex/tree/v0.2.0
endpointCodec :: RouteDuplex' Endpoint
endpointCodec = root $ sum
  { "LocaleMap": "api" / "localeMap" / noArgs
  }
