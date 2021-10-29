module Server.Main where

import Prelude

import Api as Api
import Control.Alt ((<|>))
import Data.Either (Either)
import Data.List (List, singleton)
import Data.Locale (LocaleMap, NestedNamespace, SourceType(..))
import Debug as D
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, runAff_)
import Effect.Exception (Error)
import Node.HTTP as HTTP
import Payload.ResponseTypes (Failure)
import Payload.Server as Payload
import Payload.Server.Handlers (File)
import Payload.Server.Handlers as Handlers
import Payload.Spec (Spec(Spec), GET, Routes)
import Processor.ReadDir as P


indexPage :: {} -> Aff File
indexPage = Handlers.file "public/index.html"

public :: { params :: { path :: List String } } -> Aff (Either Failure File)
public { params: {path} } = Handlers.directory "public" path

output :: { params :: { path :: List String } } -> Aff (Either Failure File)
output { params: {path} } = Handlers.directory "output" $ path <> singleton "index.js"

getLocaleMap :: { params :: { sourceType :: SourceType } } -> Aff (LocaleMap NestedNamespace)
getLocaleMap { params: { sourceType: ExtractedSource }} = P.readDir $ P.Opts { inputDir: "localeProcess/extracted" }
getLocaleMap { params: { sourceType: TranslatedSource }} = P.readDir $ P.Opts { inputDir: "locale" }

-- api ::
--     { guards :: Record ()
--     , handlers ::
--         { indexPage :: Record () -> Aff File
--         , public :: { params :: { path :: List String } } -> Aff (Either Failure File)
--         -- , output :: { params :: { path :: List String } } -> Aff (Either Failure File)
--         }
--     }


main :: Effect Unit
main = launchAff_ $ Payload.startGuarded (Payload.defaultOpts { port = 3001, logLevel = Payload.LogDebug }) Api.spec api
  where
    api =
      { handlers: 
        { indexPage
        , public
        , api: 
          { localeMap: 
            { get: getLocaleMap }
          }
        }
      , guards: {} 
      }


foreign import connector :: (HTTP.Request -> HTTP.Response -> Effect Unit) -> Effect Unit

-- main2 :: (Either Error (Either String Payload.Server) -> Effect Unit) -> Effect Unit
-- main2 cb = runAff_ cb $ Payload.startGuarded_ spec api

-- main3 :: Effect Unit
-- main3 = launchAff_ $ Payload.connectServer_ connector spec api
