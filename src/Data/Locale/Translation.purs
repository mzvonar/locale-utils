module Data.Locale.Translation where

import Prelude

import Data.Array (partition) as Array
import Data.Array.NonEmpty (NonEmptyArray, fromArray, catMaybes)
import Data.Either (either)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Locale (LocaleMap, Namespace(..), NamespaceName, TranslationKey, TranslationValue(..))
import Data.Map (Map, fromFoldable, unionWith)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set (fromFoldable) as Set
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (slice)
import Data.String.Common (replace)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.String.Regex (Regex, regex, match)
import Data.String.Regex.Flags (global)
import Data.String.Utils (endsWith)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafeCrashWith)

type TranslationVariables = Maybe (Set String)

data Translation 
  = SingularTranslation TranslationKey { value :: String, variables :: TranslationVariables }
  | PluralTranslation TranslationKey 
      { singular :: { value :: String, variables :: TranslationVariables }
      , plural :: { value :: String, variables :: TranslationVariables }
      }

data TranslationNamespace = TranslationNamespace NamespaceName (Map TranslationKey Translation)

derive instance Generic Translation _
instance Show Translation where
  show = genericShow

derive instance Generic TranslationNamespace _
instance Show TranslationNamespace where
  show = genericShow

fromLocaleMap :: LocaleMap Namespace -> LocaleMap TranslationNamespace
fromLocaleMap = mapWithIndex fromNamespace

key :: Translation -> TranslationKey
key (SingularTranslation k _) = k
key (PluralTranslation k _) = k

namespaceKey :: TranslationNamespace -> NamespaceName
namespaceKey (TranslationNamespace k _) = k

variables :: Translation -> { singular :: TranslationVariables, plural :: TranslationVariables }
variables (SingularTranslation _ { variables: v }) = { singular: v, plural: Nothing }
variables (PluralTranslation _ { singular: { variables: v }, plural: {variables: pv }}) = { singular: v, plural: pv }

fromNamespace :: NamespaceName -> Namespace -> TranslationNamespace
fromNamespace n (Namespace ts) =
  let
    { no: singulars, yes: plurals } = Array.partition isPlural ts
    singulars' = fromFoldable $ map (\t@(TranslationValue k _) -> Tuple k $ fromTranslationValue t) singulars
    plurals' = fromFoldable $ map (\t@(TranslationValue k _) -> Tuple (removePlural k) $ fromTranslationValue t) plurals
    translations = unionWith joinPlural singulars' plurals'
  in
    TranslationNamespace n translations
  
  where      
    isPlural :: TranslationValue -> Boolean
    isPlural (TranslationValue k _) = endsWith "_plural" k

    removePlural :: String -> String
    removePlural = replace (Pattern "_plural") (Replacement "")

    joinPlural :: Translation -> Translation -> Translation
    joinPlural (SingularTranslation k v1) (SingularTranslation _ v2) = PluralTranslation k { singular: v1, plural: v2 }
    joinPlural _ _ = unsafeCrashWith "This should not contain PluralTranslation"


fromTranslationValue :: TranslationValue -> Translation
fromTranslationValue (TranslationValue k v) = SingularTranslation k { value: v, variables: parseVariables v }

toTranslationValue :: Translation -> Array TranslationValue
toTranslationValue (SingularTranslation k { value }) = [ TranslationValue k value ]
toTranslationValue (PluralTranslation k { singular: { value }, plural: { value: pValue } }) = 
  [ TranslationValue k value
  , TranslationValue (k <> "_plural") pValue
  ]

variablesRe :: Regex
variablesRe = either unsafeCrashWith identity $ regex """{{(\w+)}}""" global

parseVariables :: String -> TranslationVariables
parseVariables s = parse $ match variablesRe s
  where
    parse :: Maybe (NonEmptyArray (Maybe String)) -> TranslationVariables
    parse Nothing = Nothing
    parse (Just matches) = matches # map stripBraces >>> catMaybes >>> fromArray >>= Set.fromFoldable >>> pure

    stripBraces :: Maybe String -> Maybe String
    stripBraces = (=<<) (slice 2 (-2))