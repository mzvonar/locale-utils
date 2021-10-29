module Client.Component.Locale where
  
import Prelude

import Client.Component.Tabs as Tabs
import Client.Component.Utils as Utils
import Data.Locale (Locale(..), NamespaceName, NestedNamespace(..), NestedTranslationValue(..))
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))

type Slot id = forall q. H.Slot q Void id
_label = Proxy :: Proxy "locale"

type State =
  { locale :: Locale NestedNamespace
  }

type Input = State

data Action
  = Receive Input

type Slots = 
  ( tabs :: Tabs.Slot String Unit
  )

component :: forall query output m. MonadAff m => H.Component query Input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval 
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
    }

  where  
  initialState :: Input -> State
  initialState = identity

  render :: State -> H.ComponentHTML Action Slots m
  render st =
    let 
      mTabProps = getTabProps st.locale
    in
      HH.div_ [ maybe (HH.text "Nothing loaded") (\tp -> HH.slot_ Tabs._label unit Tabs.component tp) mTabProps ]
        

    where
      getTabProps :: Locale NestedNamespace -> Maybe (Tabs.Props NamespaceName query output m)
      getTabProps (Locale l) = Utils.tabsPropsFromMap (Utils.Element namespace) l

      namespace :: forall w i. NestedNamespace -> HH.HTML w i
      namespace (NestedNamespace ts) = HH.div_ $ map translation ts

      translation :: forall w i. NestedTranslationValue -> HH.HTML w i
      translation (NestedTranslationValue k v) = HH.div_
        [ HH.b_ [ HH.text $ k <> ": " ]
        , HH.text v
        ]
      translation (TranslationParent k ts) = HH.div_
        [ HH.div_ [ HH.b_ [ HH.text $ k <> ": " ] ]
        , HH.div_ $ map translation ts
        ]

  handleAction :: MonadAff m => Action -> H.HalogenM State Action Slots output m Unit
  handleAction = case _ of
    Receive input ->
      H.modify_ _ { locale = input.locale }