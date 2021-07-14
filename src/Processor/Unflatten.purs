module Processor.Unflatten where

import Prelude

import Data.Array (concat, cons, foldl, findIndex, updateAt, (!!))
import Data.Either (Either(..))
import Data.Function (on)
import Data.List (List(..), fromFoldable, (:))
import Data.Locale (LocaleMap(..), Namespace(..), TranslationKey, TranslationValue(..))
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..))
import Data.String.Common (split)
import Data.Traversable (sequence)
import Partial (crashWith)
import Partial.Unsafe (unsafePartial)


unflatten :: LocaleMap Namespace -> Either String (LocaleMap Namespace)
unflatten lm = do
  let (LocaleMap lm') = map unflattenNamespace lm
  res <- sequence $ map sequence lm'
  pure $ LocaleMap res

  where
    unflattenNamespace :: Namespace -> Either String Namespace
    unflattenNamespace (Namespace as) = do
      unflattened <- pure <<< concat =<< (sequence $ map unflattenTranslation as)
      res <- foldl foldParents (Right []) unflattened
      pure $ Namespace res

    unflattenTranslation :: TranslationValue -> Either String (Array TranslationValue)
    unflattenTranslation (TranslationValue k v) = Right [constructParent (fromFoldable $ split (Pattern ".") k) v]
    unflattenTranslation _ = Left "Flattened translations contained TranslationParent"

    foldParents :: Either String (Array TranslationValue) -> TranslationValue -> Either String (Array TranslationValue)
    foldParents (Left e) _ = Left e
    foldParents (Right acc) t =
      case findIndex (keyEq t) acc of
        Nothing -> Right $ t `cons` acc
        Just i  -> do
          parent <- maybe (Left "Couldn't find index that should be ther") Right $ acc !! i
          parent' <- concatTrans parent t
          maybe (Left "Couldn't modify acc") Right $ updateAt i parent' acc

    keyEq :: TranslationValue -> TranslationValue -> Boolean
    keyEq = eq `on` key

    concatTrans :: TranslationValue -> TranslationValue -> Either String TranslationValue
    concatTrans (TranslationValue k1 _)   (TranslationValue k2 _)   | k1 == k2  = Left $ "Duplicate translation key " <> k1
    concatTrans (TranslationParent k1 v1) (TranslationParent k2 v2) | k1 /= k2 = Left $ "Can't concat parents with mismatching keys " <> k1 <> " and " <> k2
                                                                    | otherwise = Right $ TranslationParent k1 (v1 <> v2)
    concatTrans a b | a `keyEq` b = Left $ "Invalid translation key " <> key a <> ", a parent with this key already exists"
                    | otherwise = Left $ "Can't concat " <> show a <> " with " <> show b

    key :: TranslationValue -> TranslationKey
    key (TranslationValue k _) = k
    key (TranslationParent k _) = k

    constructParent :: List String -> String -> TranslationValue
    constructParent Nil _ = unsafePartial $ crashWith "Key must contain at least one segment"
    constructParent (h:Nil) v = TranslationValue h v
    constructParent (h:t) v = TranslationParent h [constructParent t v]