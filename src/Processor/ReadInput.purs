module Processor.ReadInput where

import Prelude

import Data.Argonaut (decodeJson, parseJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Locale (LocaleMap, NestedNamespace)
import Data.String (trim)
import Effect.Aff (Aff, makeAff, throwError, nonCanceler, effectCanceler)
import Effect.Exception (error)
import Effect.Ref as Ref
import Node.Encoding (Encoding(..))
import Node.Path (FilePath)
import Node.Process (stdin, stdinIsTTY)
import Node.Stream (Readable, onDataString, onEnd, onError, pause)
import Processor.ReadDir (Opts(..), readDir) as P

data Input
  = DirInput FilePath
  | StdInput

derive instance Generic Input _
instance Show Input where
  show = genericShow

readInput :: Input -> Aff (LocaleMap NestedNamespace)
readInput (DirInput inputDir) = P.readDir $ P.Opts { inputDir }
readInput StdInput = do
  stdInRes <- readText stdin
  decoded <- pure do
      text <- stdInRes
      parsed <- lmap show $ parseJson $ trim text
      lmap show $ decodeJson parsed
  -- decoded <- (lmap show (decodeJson <=< parseJson)) =<< stdInRes
  case decoded of
    Right v -> pure v
    Left e -> throwError $ error e
  
  -- case text of
  --   (Just text') -> pure $ decodeJson $ parseJson text'
  --   _ -> throwError $ error "Couldn't read stdin"

readText :: forall w. Readable w -> Aff (Either String String)
readText r = makeAff $ \res ->
  if stdinIsTTY then do
    res $ Right $ Left "Stdin is TTY"
    pure nonCanceler
  else do
    dataRef <- Ref.new ""
    onDataString r UTF8 \chunk ->
      Ref.modify_ (_ <> chunk) dataRef
    onEnd r do
      allData <- Ref.read dataRef
      res $ Right (Right allData)
    onError r $ Left >>> res
    pure $ effectCanceler (pause r)