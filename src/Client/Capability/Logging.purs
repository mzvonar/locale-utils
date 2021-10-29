module Client.Capability.Logging where

import Prelude

import Client.Capability.Now (class Now)
import Client.Data.Log (Log, LogType(..), mkLog)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Halogen (HalogenM)


class Monad m <= Logging m where
  logMessage :: Log -> m Unit

-- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance Logging m => Logging (HalogenM st act slots msg m) where
  logMessage = lift <<< logMessage


-- | Log a message to given a particular `LogType`
log :: forall m. Logging m => Now m => LogType -> String -> m Unit
log logType = logMessage <=< mkLog logType

-- | Log a message for debugging purposes
logDebug :: forall m. Logging m => Now m => String -> m Unit
logDebug = log Debug

-- | Log a message to convey non-error information
logInfo :: forall m. Logging m => Now m => String -> m Unit
logInfo = log Info

-- | Log a message as a warning
logWarn :: forall m. Logging m => Now m => String -> m Unit
logWarn = log Warn

-- | Log a message as an error
logError :: forall m. Logging m => Now m => String -> m Unit
logError = log Error

-- | Hush a monadic action by logging the error, leaving it open why the error is being logged
logHush :: forall m a. Logging m => Now m => LogType -> m (Either String a) -> m (Maybe a)
logHush logType action =
  action >>= case _ of
    Left e -> case logType of
      Debug -> logDebug e *> pure Nothing
      Info -> logInfo e *> pure Nothing
      Warn -> logWarn e *> pure Nothing
      Error -> logError e *> pure Nothing
    Right v -> pure $ Just v

-- | Hush a monadic action by logging the error in debug mode
debugHush :: forall m a. Logging m => Now m => m (Either String a) -> m (Maybe a)
debugHush = logHush Debug
