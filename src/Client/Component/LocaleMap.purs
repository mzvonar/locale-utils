module Client.Component.LocaleMap where
  
import Prelude

import Client.Capability.Resource.LocaleMap (class ManageLocaleMap, getLocaleMap)
import Client.Component.Locale as Locale
import Client.Component.Tabs as Tabs
import Client.Component.Utils as Utils
import Data.Either (hush)
import Data.Locale (LocaleMap(..), LocaleName, NestedNamespace, SourceType(..))
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))


type Slots = 
  ( tabs :: Tabs.Slot String Unit
  -- ( tabs :: forall k. Tabs.Slot k Unit
  , locale :: Locale.Slot Unit
  )

type State =
  { loading :: Boolean
  , data :: Maybe (LocaleMap NestedNamespace)
  }

data Action
  = Initialize
  | LoadLocales SourceType

_label = Proxy :: Proxy "localeMap"

component :: forall query input output m. MonadAff m => ManageLocaleMap m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval 
      { handleAction = handleAction 
      , initialize = Just Initialize
      }
    }

  where  
  initialState :: input -> State
  initialState _ = { loading: false, data: Nothing }

  render :: State -> H.ComponentHTML Action Slots m
  render st =
    HH.div_ if st.loading 
      then [ HH.text "Loading..."  ]
      else 
        let 
          lm = st.data
          mTabProps = getLMTabProps lm
        in [
        HH.div_ 
        [ HH.h1_ [ HH.text "Localizator" ]
        , maybe (HH.text "Nothing loaded") (\tp -> HH.slot_ Tabs._label unit Tabs.component tp) mTabProps
        ]
      ]
    where
      getLMTabProps :: forall cq co. Maybe (LocaleMap NestedNamespace) -> Maybe (Tabs.Props LocaleName cq co m)
      getLMTabProps (Just (LocaleMap lm)) = Utils.tabsPropsFromMap (Utils.Component ({ component: Locale.component, getInput: \locale -> { locale } })) lm
      getLMTabProps Nothing = Nothing

  handleAction :: MonadAff m => ManageLocaleMap m => Action -> H.HalogenM State Action Slots output m Unit
  handleAction = case _ of
    Initialize -> do
      handleAction $ LoadLocales ExtractedSource

    LoadLocales t -> do
      H.modify_ _ { loading = true }
      response <- getLocaleMap t
      H.modify_ _ { loading = false, data = hush response }