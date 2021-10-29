module Client.Component.Root where

import Prelude

import Client.Capability.Resource.LocaleMap (class ManageLocaleMap)
import Client.Component.LocaleMap as LocaleMap
import Client.Component.Utils (OpaqueSlot)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH

type ChildSlots =
  ( localeMap :: OpaqueSlot Unit
  )

component 
  :: forall query input output m
   . MonadAff m 
  => ManageLocaleMap m 
  => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
    }

  where
  initialState = identity

  render :: forall state action. state -> H.ComponentHTML action ChildSlots m
  render _ = do
    HH.div_ 
      [
        HH.h3_ [ HH.text "Hello Halogen!" ]
      , HH.slot_ LocaleMap._label unit LocaleMap.component unit
      ]