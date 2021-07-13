module Command.WriteDir where

-- import Prelude

-- import Command (Input)
-- import Data.Argonaut (encodeJson, stringify)
-- import Data.Either (Either(..))
-- import Data.Foldable (fold)
-- import Data.Locale (LocaleMap)
-- import Effect (Effect)
-- import Effect.Aff (Aff, throwError, runAff_)
-- import Effect.Console (log)
-- import Effect.Class (liftEffect)
-- import Effect.Exception (error)
-- import Node.Path (concat, isAbsolute, normalize)
-- import Node.Process (cwd)
-- import Options.Applicative (Parser, (<**>), helper, fullDesc, progDesc, info, strOption, long, short, metavar, help, execParser, showDefault, value)
-- import Processor.WriteDir (Opts(..), writeDir) as P

-- data Opts = Opts 
--   { input :: Input
--   , outputDir :: String
--   }

-- writeDirOpts :: Parser Opts
-- writeDirOpts = ado
--   outputDir <- strOption $ fold
--                 [ long "output-dir"
--                 , short 'o'
--                 , metavar "STRING"
--                 , help "Output directory to write results to"
--                 , showDefault
--                 , value "./locale-output"
--                 ]
--   in Opts { outputDir }

-- writeDir :: Aff Unit
-- writeDir = P.writeDir =<< liftEffect (fixPath =<< execParser opts)
--   where
--     opts = info (writeDirOpts <**> helper)
--       ( fullDesc
--      <> progDesc "Writes results to directory with locale subdirectries which contains namespace files"
--       )

--     fixPath :: Opts -> Effect Opts
--     fixPath (Opts o) = do
--       baseDir <- cwd
--       pure $ if isAbsolute o.outputDir 
--         then Opts o 
--         else Opts (o {outputDir = normalize $ concat [baseDir, o.outputDir]})

-- -- main :: Effect Unit
-- -- main = launchAff_ do
-- --     eitherRes <- readDir
-- --     case eitherRes of
-- --         Right res -> log $ stringify $ encodeJson res
-- --         Left e -> throwError $ error e

-- main :: Effect Unit
-- main = runAff_ (\res -> do
--   case res of
--     Right res -> pure unit
--     Left e -> throwError $ error e
--   ) writeDir

