-- From https://github.com/nonbili/purescript-halogen-ntabs
module Client.Component.Tabs
  ( Component
  , Slot
  , _label
  , Message(..)
  , Query
  , Action
  , Props
  , Slots
  , _tabContent
  , HTML
  , setTabProps
  , component
  ) where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))
import Web.UIEvent.MouseEvent as ME

type Slot k id = H.Slot Query (Message k) id
_label = Proxy :: Proxy "tabs"

type Props v cq co m =
  { render :: v -> HTML v cq co m
  , initial :: v
  , value :: Maybe v
  }

type Message :: forall k. k -> k
type Message v = v

type Query :: forall k. k -> Type
type Query = Const Void

data Action v cq co m
  = OnClick v
  | Receive (Props v cq co m)

type HTML v cq co m = H.ComponentHTML (Action v cq co m) (Slots cq co) m

type DSL v cq co m = H.HalogenM (State v cq co m) (Action v cq co m) (Slots cq co) (Message v) m

type State v cq co m =
  { props :: Props v cq co m
  , value :: v
  }

type TabProps r =
  ( onClick :: ME.MouseEvent
  | r
  )

type Slots q o = 
  ( tabContent :: H.Slot q o Unit
  )
_tabContent = Proxy :: Proxy "tabContent"

type Component v cq co m = H.Component Query (Props v cq co m) (Message v) m


initialState :: forall v cq co m. Props v cq co m -> State v cq co m
initialState props =
  { props
  , value: fromMaybe props.initial props.value
  }

setTabProps
  :: forall v cq co m r
   . v
  -> Array (HH.IProp (TabProps r) (Action v cq co m))
  -> Array (HH.IProp (TabProps r) (Action v cq co m))
setTabProps v props = props <>
  [ HE.onClick $ const (OnClick v)
  ]

render :: forall v cq co m. State v cq co m -> HTML v cq co m
render state =
  HH.div_
  [ state.props.render state.value
  ]

component :: forall v cq co m. Component v cq co m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
  }

handleAction :: forall v cq co m. Action v cq co m -> DSL v cq co m Unit
handleAction = case _ of
  OnClick value -> do
    H.modify_ $ _ { value = value }
    H.raise value

  Receive props -> do
    H.modify_ \s -> s
      { props = props
      , value = fromMaybe s.value props.value
      }