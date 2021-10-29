module Client.Component.TabsExample where

-- import Prelude

-- import Client.Component.Tabs as Tabs
-- import Data.Const (Const)
-- import Data.Maybe (Maybe(..))
-- import Effect.Class (class MonadEffect)
-- import Halogen as H
-- import Halogen.HTML as HH
-- import Halogen.HTML.Properties as HP
-- import Type.Proxy (Proxy(..))

-- type Slot id = forall q. H.Slot q Void id

-- type Action = Void

-- type Slots =
--   ( tabs :: H.Slot Tabs.Query (Tabs.Message Int) Unit
--   )

-- _tabs = Proxy :: Proxy "tabs"

-- type HTML = forall m. MonadEffect m => H.ComponentHTML Action Slots m


-- class_ :: forall r i. String -> HH.IProp ("class" :: String | r) i
-- class_ = HP.class_ <<< HH.ClassName

-- style :: forall r i. String -> HH.IProp ("style" :: String | r) i
-- style = HH.attr (HH.AttrName "style")

-- renderTabs :: forall m. MonadEffect m => Int -> Tabs.HTML Int m
-- renderTabs selected =
--   HH.div_
--   [ HH.ul
--     [ class_ "flex"]
--     [ HH.li
--       ( Tabs.setTabProps 0
--         [ class_ $ getTabCls 0
--         ])
--       [ HH.text "Tab 0"]
--     , HH.li
--       ( Tabs.setTabProps 1
--         [ class_ $ getTabCls 1
--         ])
--       [ HH.text "Tab 1"]
--     ]
--   , HH.div
--     [ class_ "mt-1 border p-4"]
--     [ case selected of
--          0 -> HH.text "tab 0 content"
--          1 -> HH.text "tab 1 content"
--          _ -> HH.text ""
--     ]
--   ]
--   where
--   baseCls = "px-3 cursor-pointer "
--   inactiveCls = "text-gray-500"
--   activeCls = "text-black border-b border-blue-500"
--   getTabCls n = baseCls <> if (n == selected) then activeCls else inactiveCls

-- render :: forall s m. MonadEffect m => s -> H.ComponentHTML Action Slots m
-- render state =
--   HH.div
--     [ class_ ""]
--     [ HH.slot_ _tabs unit Tabs.component
--         { render: renderTabs
--         , initial: 0
--         , value: Nothing
--         }
--     , HH.div
--         [ class_ "mt-4"]
--         [ HH.div
--           [ class_ "mb-1"]
--           [ HH.text "Tabs manage its own state."]
--         ]
--     ]

-- component :: forall query input m. MonadEffect m => H.Component query input Void m
-- component = H.mkComponent
--   { initialState: identity
--   , render
--   , eval: H.mkEval H.defaultEval
--   } 