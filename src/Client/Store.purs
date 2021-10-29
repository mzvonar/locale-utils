module Client.Store where

import Prelude

import Client.Api.Request (BaseURL)
import Data.Maybe (Maybe(..))

data LogLevel = Dev | Prod

derive instance Eq LogLevel
derive instance Ord LogLevel

type Store =
  { logLevel :: LogLevel
  , baseUrl :: BaseURL
  }

data Action
  = NoAction

reduce :: Store -> Action -> Store
reduce store = case _ of
  NoAction -> store