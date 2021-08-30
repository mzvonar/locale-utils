module Test.Free where

import Prelude

import Control.Monad.Free (Free, liftF, foldFree, resume')
import Control.Monad.Reader (ReaderT, ask, lift, local, runReaderT)
import Effect (Effect)
import Effect.Console as Console

data Filter = All | Info | Error
derive instance Eq Filter
instance Show Filter where
  show All = "All"
  show Info = "Info"
  show Error = "Error"

data ReportF a
  = SetFilter Filter a
  | ShowFilter a

derive instance Functor ReportF

type Report = Free ReportF

type Env = Filter
type Output a = ReaderT Env Effect a

setFilter :: Filter -> Report Unit
setFilter a = liftF $ SetFilter a unit

showFilter :: Report Unit
showFilter = liftF $ ShowFilter unit

renderToConsole :: Report Unit -> Effect Unit
renderToConsole r = runReaderT (interpret r) All

-- foldrFree :: forall f m a. Functor f => Monad m => (forall x. f (m x) -> m x) -> Free f a -> m a
-- foldrFree :: forall f a r. (forall b. f b -> (b -> Free f a) -> r) -> Free f a -> 
-- foldrFree k = resume' go identity k
--   where
--     -- go :: (forall x. f (m x) -> m x) -> Free f a -> m a
--     go f = case _ of
--       SetFilter a next -> do
--         lift $ Console.log $ "Setting filter to " <> show a
--         local (const a) do
--           filter <- ask
--           -- The filter here is set properly
--           lift $ Console.log $ "Inside the setFilter fn the filter is " <>show filter
--           -- Filter inside this next computation is lost
--           next
--       ShowFilter next -> do
--         filter <- ask
--         lift $ Console.log $ "Filter is now " <> show filter
--         next

-- interpret' :: Report ~> Output
-- interpret' = foldrFree go
--   where
--     go :: forall x. f (m x) -> m x
--     go = case _ of
--       SetFilter a next -> do
--         lift $ Console.log $ "Setting filter to " <> show a
--         local (const a) do
--           filter <- ask
--           -- The filter here is set properly
--           lift $ Console.log $ "Inside the setFilter fn the filter is " <>show filter
--           -- Filter inside this next computation is lost
--           next
--       ShowFilter next -> do
--         filter <- ask
--         lift $ Console.log $ "Filter is now " <> show filter
--         next

interpret :: Report ~> Output
interpret = foldFree go
  where
  go :: ReportF ~> Output
  go = case _ of
    SetFilter a next -> do
      lift $ Console.log $ "Setting filter to " <> show a
      local (const a) do
        filter <- ask
        -- The filter here is set properly
        lift $ Console.log $ "Inside the setFilter fn the filter is " <>show filter
        -- Filter inside this next computation is lost
        pure next
    ShowFilter next -> do
      filter <- ask
      lift $ Console.log $ "Filter is now " <> show filter
      pure next


showReport :: Report Unit
showReport = do
  setFilter Error
  showFilter

main :: Effect Unit
main = renderToConsole showReport