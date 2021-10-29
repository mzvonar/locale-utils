module Processor.Merge where

import Prelude

import Data.Array as Array
import Data.Function (on)
import Data.Locale (LocaleMap(..), Locale(..), Namespace(..), key)
import Data.Map as Map

merge :: LocaleMap Namespace -> LocaleMap Namespace -> LocaleMap Namespace
merge (LocaleMap slm) (LocaleMap tlm) = LocaleMap $ Map.unionWith mergeLocales slm tlm
  where
    mergeLocales :: Locale Namespace -> Locale Namespace -> Locale Namespace
    mergeLocales (Locale sl) (Locale tl) = Locale $ Map.unionWith mergeNamespaces sl tl

    mergeNamespaces :: Namespace -> Namespace -> Namespace
    mergeNamespaces (Namespace svs) (Namespace tvs) = Namespace $ Array.unionBy (eq `on` key) tvs svs