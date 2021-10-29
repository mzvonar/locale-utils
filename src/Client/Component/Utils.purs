-- | Some utilities are useful across any component. We'll maintain them in this catch-all module.
module Client.Component.Utils where

import Prelude

import Client.Component.Tabs as Tabs
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

-- | When a component has no queries or messages, it has no public interface and can be
-- | considered an "opaque" component. The only way for a parent to interact with the
-- | component is by sending input.
type OpaqueSlot slot = forall query. H.Slot query Void slot


data RenderContent a query input output m
 = Element (forall w i. a -> HH.HTML w i)
 | Component { component :: H.Component query input output m, getInput :: a -> input }

class_ :: forall r i. String -> HH.IProp ("class" :: String | r) i
class_ = HP.class_ <<< HH.ClassName

tabsPropsFromMap 
  :: forall k a cq ci co m
   . MonadAff m => Eq k => Ord k => Show k 
  => RenderContent a cq ci co m -> Map k a -> Maybe (Tabs.Props k cq co m)
tabsPropsFromMap render mp = 
  case List.head $ Set.toUnfoldable $ Map.keys mp of
    Nothing -> Nothing
    Just firstKey -> Just
      { render: renderTabs mp render
      , initial: firstKey
      , value: Nothing
      }

  where
    renderTabs :: Map k a -> RenderContent a cq ci co m -> k -> Tabs.HTML k cq co m
    renderTabs value renderTabContent selected =
      HH.div_
      [ HH.ul
        [ class_ "flex"]
        $ map (renderTab selected) $ Set.toUnfoldable $ Map.keys value 
      , HH.div
        [ class_ "mt-1 border p-4"]
        [ 
          case Map.lookup selected value of
            Nothing -> HH.text "Couldn't load tab"
            Just a -> case renderTabContent of
              Element f -> f a
              Component { component, getInput } -> HH.slot_ Tabs._tabContent unit component (getInput a)
        ]
      ]

    renderTab :: k -> k -> Tabs.HTML k cq co m
    renderTab selected k = HH.li
      ( Tabs.setTabProps k
        [ class_ $ getTabCls selected k
        ])
      [ HH.text $ show k]

    baseCls = "px-3 cursor-pointer "
    inactiveCls = "text-gray-500"
    activeCls = "text-black border-b border-blue-500"

    getTabCls :: k -> k -> String  
    getTabCls selected n = baseCls <> if (n == selected) then activeCls else inactiveCls