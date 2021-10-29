module Client.AppM where

import Prelude

import Api as Api
import Client.Capability.Logging (class Logging, logError)
import Client.Capability.Now (class Now)
import Client.Capability.Resource.LocaleMap (class ManageLocaleMap)
import Client.Data.Log as Log
import Client.Store (Action, LogLevel(..), Store)
import Client.Store as Store
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Now as Now
import Halogen as H
import Halogen.Store.Monad (class MonadStore, StoreT, getStore, runStoreT)
import Payload.Client as Payload
import Payload.Client.ClientApi (class ClientApi)
import Safe.Coerce (coerce)

newtype AppM a = AppM (StoreT Store.Action Store.Store Aff a)

runAppM :: forall q i o. Store.Store -> H.Component q i o AppM -> Aff (H.Component q i o Aff)
runAppM store = runStoreT store Store.reduce <<< coerce

derive newtype instance Functor AppM
derive newtype instance Apply AppM
derive newtype instance Applicative AppM
derive newtype instance Bind AppM
derive newtype instance Monad AppM
derive newtype instance MonadEffect AppM
derive newtype instance MonadAff AppM
derive newtype instance MonadStore Action Store AppM


instance Now AppM where
  now = liftEffect Now.now
  nowDate = liftEffect Now.nowDate
  nowTime = liftEffect Now.nowTime
  nowDateTime = liftEffect Now.nowDateTime


instance Logging AppM where
  logMessage log = do
    { logLevel } <- getStore
    liftEffect case logLevel, Log.logType log of
      Prod, Log.Debug -> pure unit
      _, _ -> Console.log $ Log.message log

client = Payload.mkGuardedClient_ Api.spec

-- | Our operations for managing articles
instance ManageLocaleMap AppM where
  getLocaleMap t = do
    -- eJson <- mkRequest { endpoint: LocaleMap, method: Get }
    -- let mbJson = hush ?eJson
    -- lm <- decode' mbJson
    -- pure $ note "Couldnt' decode locale map" lm
    localeMap <- liftAff $ Payload.unwrapBody $ client.api.localeMap.get { params: { sourceType: t } }
    pure $ Right localeMap

  updateLocaleMap lm = do
    pure $ Right lm

withLog 
  :: forall m e a
   . Logging m 
  => Now m
  => Show e 
  => m (Either e a) -> m (Either e a)
withLog m = bind m \v -> case v of
  Left e -> do
    logError $ show e
    pure v
  _ -> pure v
  