-- | This module exports various utilities for working with a REST API and Json. It also provides
-- | a few helpers shared among requests which I found useful when implementing the production
-- | monad, `Conduit.AppM`.
module Client.Api.Utils where

import Prelude

import Affjax (printError, request)
import Client.Api.Request (BaseURL, RequestOptions, Token, defaultRequest, readToken, writeToken)
import Client.Capability.Logging (class Logging, logError)
import Client.Capability.Now (class Now)
import Client.Store (Action(..), Store)
import Data.Argonaut (JsonDecodeError, decodeJson, parseJson, printJsonDecodeError)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Bifunctor (bimap)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen.Store.Monad (class MonadStore, getStore, updateStore)

type Error = String

mkRequest
  :: forall m
   . MonadAff m
  => MonadStore Action Store m
  => RequestOptions
  -> m (Either Error Json)
mkRequest opts = do
  { baseUrl } <- getStore
  response <- liftAff $ request $ defaultRequest baseUrl Nothing opts
  pure $ bimap printError _.body response



-- | This small utility decodes JSON and logs any failures that occurred, returning the parsed
-- | value only if decoding succeeded. This utility makes it easy to abstract the mechanices of
-- | dealing with malformed responses. See `Conduit.AppM` for examples of this in practice.
decode :: forall m a. Logging m => Now m => JsonCodec a -> Maybe Json -> m (Maybe a)
decode _ Nothing = logError "Response malformed" *> pure Nothing
decode codec (Just json) = case CA.decode codec json of
  Left err -> logError (CA.printJsonDecodeError err) *> pure Nothing
  Right response -> pure (Just response)

decode' :: forall m a. Logging m => Now m => DecodeJson a => Maybe Json -> m (Maybe a)
decode' Nothing = logError "Response malformed" *> pure Nothing
decode' (Just json) = case decodeJson json of
  Left err -> logError (printJsonDecodeError err) *> pure Nothing
  Right response -> pure (Just response)