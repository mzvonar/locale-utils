module Client.Data.Log
  ( LogType(..)
  , message
  , logType
  , Log
  , mkLog
  ) where

import Prelude

import Client.Capability.Now (class Now, nowDateTime)
import Data.DateTime (DateTime)
import Data.Either (either)
import Data.Foldable (fold)
import Data.Formatter.DateTime (formatDateTime)


data LogType = Debug | Info | Warn | Error

derive instance Eq LogType
derive instance Ord LogType

newtype Log = Log
  { logType :: LogType
  , timestamp :: DateTime
  , message :: String
  }

derive instance Eq Log

message :: Log -> String
message (Log { message }) = message

logType :: Log -> LogType
logType (Log { logType }) = logType

mkLog :: forall m. Now m => LogType -> String -> m Log
mkLog logType inputMessage = do
  timestamp <- nowDateTime

  let
    headerWith start =
      fold [ "[", start, ": ", formatTimestamp timestamp, "]: ", inputMessage ]

    formattedMessage = headerWith case logType of
      Debug -> "DEBUG"
      Info -> "INFO"
      Warn -> "WARNING"
      Error -> "ERROR"

  pure $ Log { logType, timestamp, message: formattedMessage }

  where

  formatTimestamp =
    either (const "(Failed to assign time)") identity
      <<< formatDateTime "YYYY-DD-MM hh:mm:ss a"
