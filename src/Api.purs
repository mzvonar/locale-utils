module Api where

import Data.Locale (LocaleMap, NestedNamespace, SourceType)

import Payload.Spec (Spec(Spec), GET, Routes)

type ApiSpec = Spec
  { guards :: {}
  , routes ::
    { 
    --   indexPage :: GET "/"
    --     { response :: File }
    -- ,
     api :: Routes "/api"
      { localeMap :: Routes "/localeMap"
        { get :: GET "/<sourceType>"
          { params :: { sourceType :: SourceType }
          , response :: LocaleMap NestedNamespace 
          }
        }
      }
    -- , public :: GET "/<..path>"
    --     { params :: { path :: List String }
    --     , response :: File }
    -- , output :: GET "/output/<..path>"
    --     { params :: { path :: List String }
    --     , response :: File }
    }
  }

spec :: ApiSpec
spec = Spec