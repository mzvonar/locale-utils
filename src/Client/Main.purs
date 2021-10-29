module Client.Main where

import Prelude

import Client.Api.Request (BaseURL(..))
import Client.AppM (runAppM)
import Client.Component.Root as Root
import Client.Store (LogLevel(..), Store)
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody

  let
    baseUrl = BaseURL ""
    logLevel = Dev

    initialStore :: Store
    initialStore = { baseUrl, logLevel }

  rootComponent <- runAppM initialStore Root.component
  runUI rootComponent unit body