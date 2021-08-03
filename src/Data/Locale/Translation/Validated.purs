module Data.Locale.Translation.Validated where

import Prelude

import Data.Array (catMaybes, null, fromFoldable)
import Data.Either (Either(..))
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.List (any) as L
import Data.Locale (LocaleMap(..), Locale(..), Namespace, NamespaceName, TranslationKey, LocaleName)
import Data.Locale.Translation (Translation(..), TranslationNamespace(..), TranslationVariables, key, namespaceKey, variables)
import Data.Map (Map, keys, lookup)
import Data.Map (fromFoldable, values) as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Set (Set, empty, member, difference, union)
import Data.Show.Generic (genericShow)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Validated (Validated(..))
import Partial.Unsafe (unsafeCrashWith)

data VariableType
  = SingularVariable
  | PluralVariable
  | OnlySingularVariable

data TranslationError
  = MissingVariable VariableType String
  | ExtraneousVariable VariableType String
  | MissingKey TranslationKey
  | ExtraneousKey TranslationKey
  | UnknownError String

data ValidTranslation = ValidTranslation Translation
data InvalidTranslation = InvalidTranslation Translation { errors :: Array TranslationError }
type ValidatedTranslation = Either InvalidTranslation ValidTranslation
type ValidatedTranslations = Map TranslationKey ValidatedTranslation

data ValidatedNamespace
  = ValidNamespace NamespaceName ValidatedTranslations
  | InvalidTranslations NamespaceName ValidatedTranslations
  | ExtraneousNamespace NamespaceName
  | MissingNamespace NamespaceName
  | UnknownNamespaceError NamespaceName String

data ValidatedLocale
  = ValidLocale LocaleName (Map NamespaceName ValidatedNamespace)
  | InvalidNamespaces LocaleName (Map NamespaceName ValidatedNamespace)
  | ExtraneousLocale LocaleName
  | MissingLocale LocaleName

data ValidatedLocaleMap
  = ValidLocaleMap (Map LocaleName ValidatedLocale)
  | InvalidLocales (Map LocaleName ValidatedLocale)

derive instance Generic VariableType _
instance Show VariableType where
  show = genericShow

derive instance Generic TranslationError _
instance Show TranslationError where
  show = genericShow

derive instance Generic ValidTranslation _
instance Show ValidTranslation where
  show = genericShow

derive instance Generic InvalidTranslation _
instance Show InvalidTranslation where
  show = genericShow
instance Semigroup InvalidTranslation where
  append (InvalidTranslation a { errors: ea }) (InvalidTranslation b { errors: eb })
    | (notEq `on` key) a b = InvalidTranslation a { errors: [UnknownError "Tried to append two InvalidTranslations of different keys"] }
    | otherwise = InvalidTranslation a { errors: ea <> eb }

derive instance Generic ValidatedNamespace _
instance Show ValidatedNamespace where
  show = genericShow

derive instance Generic ValidatedLocale _
instance Show ValidatedLocale where
  show = genericShow

derive instance Generic ValidatedLocaleMap _
instance Show ValidatedLocaleMap where show = genericShow
  

validateLocaleMap :: LocaleMap TranslationNamespace -> LocaleMap TranslationNamespace -> ValidatedLocaleMap
validateLocaleMap (LocaleMap source) (LocaleMap target) = 
  let
    allKeys = union (keys source) (keys target)
    locales = M.fromFoldable $ map (\k -> Tuple k $ validateLocale k (lookup k source) (lookup k target)) $ fromFoldable allKeys
  in
    if L.any isInvalid $ M.values locales
      then InvalidLocales locales
      else ValidLocaleMap locales
  where
    isInvalid = not isValidLocale

isValidLocale  :: ValidatedLocale -> Boolean
isValidLocale (ValidLocale _ _) = true
isValidLocale _ = false

validateLocale :: LocaleName -> Maybe (Locale TranslationNamespace) -> Maybe (Locale TranslationNamespace) -> ValidatedLocale
validateLocale n Nothing (Just _) = ExtraneousLocale n
validateLocale _ Nothing Nothing = unsafeCrashWith $ "Can't validate two non-existing locales"
validateLocale n (Just _) Nothing = MissingLocale n
validateLocale n (Just (Locale source)) (Just (Locale target)) =
  let
    allKeys = union (keys source) (keys target)
    namespaces = M.fromFoldable $ map (\k -> Tuple k $ validateNamespace (lookup k source) (lookup k target)) $ fromFoldable allKeys
  in
    if L.any isInvalid $ M.values namespaces
      then InvalidNamespaces n namespaces
      else ValidLocale n namespaces

  where
    isInvalid :: ValidatedNamespace -> Boolean
    isInvalid (ValidNamespace _ _) = false
    isInvalid _ = true

mapExtraneousKeyError :: Translation -> InvalidTranslation
mapExtraneousKeyError t = InvalidTranslation t { errors: [ExtraneousKey $ key t] }

validateNamespace :: Maybe TranslationNamespace -> Maybe TranslationNamespace -> ValidatedNamespace
validateNamespace Nothing (Just (TranslationNamespace n _)) = ExtraneousNamespace n
validateNamespace Nothing Nothing = unsafeCrashWith $ "Can't validate two non-existing namespaces"
validateNamespace (Just (TranslationNamespace n _)) Nothing = MissingNamespace n
validateNamespace (Just (TranslationNamespace k1 n1)) (Just (TranslationNamespace k2 n2)) 
  | k1 /= k2  = unsafeCrashWith $ "Comparing namespaces with different keys: " <> k1 <> ", " <> k2
  | otherwise = validateNamespace' k1 n1 n2
  where
    validateNamespace' :: NamespaceName -> Map TranslationKey Translation -> Map TranslationKey Translation -> ValidatedNamespace
    validateNamespace' name source target = 
      let
        allKeys = union (keys source) (keys target)
        translations = M.fromFoldable $ map (\k -> Tuple k $ validateTranslation (lookup k source) (lookup k target)) $ fromFoldable allKeys
        res = sequence translations
      in
        case res of
          Left _ -> InvalidTranslations name translations
          Right _ -> ValidNamespace name translations



extraneousKeyError :: Translation -> InvalidTranslation
extraneousKeyError translation = InvalidTranslation translation { errors: [ExtraneousKey $ key translation]}

missingKeyError :: Translation -> InvalidTranslation
missingKeyError translation = InvalidTranslation translation { errors: [MissingKey $ key translation]}

validateTranslation :: Maybe Translation -> Maybe Translation -> Either InvalidTranslation ValidTranslation
validateTranslation Nothing Nothing = unsafeCrashWith "Can't validate two non-existing translations"
validateTranslation Nothing (Just target) = Left $ extraneousKeyError target
validateTranslation (Just source) Nothing = Left $ missingKeyError source
validateTranslation (Just source) (Just target) = validateVariables source target
  where
    validateVariables :: Translation -> Translation -> Either InvalidTranslation ValidTranslation
    validateVariables (SingularTranslation _ { variables: sVariables }) tar@(SingularTranslation _ { variables: tVariables }) =
      let
        diff = getDiff sVariables tVariables
        errors = catMaybes $ map (\a -> validateVariable OnlySingularVariable (getItem a sVariables) (getItem a tVariables)) $ fromFoldable diff
      in
        if null errors
          then Right $ ValidTranslation tar
          else Left $ InvalidTranslation tar { errors }

    validateVariables src@(PluralTranslation _ _) tar@(PluralTranslation _ _) =
      let
        { singular: sSingularVariables, plural: sPluralVariables } = variables src
        { singular: tSingularVariables, plural: tPluralVariables } = variables tar
        singularDiff = getDiff sSingularVariables tSingularVariables
        pluralDiff = getDiff sPluralVariables tPluralVariables
        singularErrors = catMaybes $ map (\a -> validateVariable SingularVariable (getItem a sSingularVariables) (getItem a tSingularVariables)) $ fromFoldable singularDiff
        pluralErrors = catMaybes $ map (\a -> validateVariable PluralVariable (getItem a sPluralVariables) (getItem a tPluralVariables)) $ fromFoldable pluralDiff
        errors = singularErrors <> pluralErrors
      in
        if null errors
          then Right $ ValidTranslation tar
          else Left $ InvalidTranslation tar { errors }
    validateVariables a b = unsafeCrashWith $ "Can't compare variables of " <> show a <> " with variables of " <> show b

    getDiff :: TranslationVariables -> TranslationVariables -> Set String
    getDiff a b = difference (fromMaybe empty a) (fromMaybe empty b) <> difference (fromMaybe empty b) (fromMaybe empty a)

    getItem :: String -> Maybe (Set String) -> Maybe String
    getItem _ Nothing = Nothing
    getItem a (Just s) = if member a s then Just a else Nothing

    validateVariable :: VariableType -> Maybe String -> Maybe String -> Maybe TranslationError
    validateVariable _ Nothing Nothing = unsafeCrashWith "Can't validate two non-existing variables"
    validateVariable t Nothing (Just tar) = Just $ ExtraneousVariable t tar
    validateVariable t (Just src) Nothing = Just $ MissingVariable t src
    validateVariable _ _ _ = Nothing