{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad.Catch
import qualified Data.Text.IO as T
import Data.Time
import Options.Applicative
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.IO.Temp (createTempDirectory)
import Turtle hiding (FilePath, header, option)

{-
  A poi launcher with some random customization.

  List of things implemented:

  - launch poi from a fresh directory every lifecycle,
    with stdout and stderr piped to files in case of crash.

  - prevent multiple instances from running.

  - switch between prod and devel version.
    (the future plan is to somehow compile and run from there automatically,
    but I don't need that feature for anything yet).

  TODO:

  - for now the tmp directory structure is a bit messy, want a few improvements
    on this front:

    + contain everything in /tmp/poi-launcher-{user}/
      (if possible set this to 700 mod)

    + create directory based on local time rather than a messy timestamp

    + let's enforce that time by second is good enough - if a directory
      by that name already exist, refuse to continue rather than trying to
      ensure uniqueness by some complicated approach.

 -}

data RawOption = RawOption
  { poiPath :: FilePath
  , electronPath :: FilePath
  , poiModeRaw :: String
  }

pOpts :: ParserInfo RawOption
pOpts =
  info
    (rawOpts <**> helper)
    ( fullDesc
        <> header "a poi launcher"
    )
  where
    rawOpts :: Parser RawOption
    rawOpts =
      RawOption
        <$> strOption
          ( long "poi-path"
              <> metavar "PATH"
              <> help "poi app path"
          )
        <*> strOption
          ( long "electron-path"
              <> metavar "PATH"
              <> help "electron bin path"
          )
        <*> option
          str
          ( long "mode"
              <> metavar "MODE"
              <> showDefault
              <> value "prod"
              <> help "poi launch mode, must be either \"prod\" or \"devel\""
          )

getCurTimestamp :: IO String
getCurTimestamp =
  formatTime defaultTimeLocale "%_Y%m%d%H%M%S" <$> getZonedTime

data PoiConf = PoiConf
  { poiPath :: FilePath
  , electronPath :: FilePath
  , hOut :: Handle
  , hErr :: Handle
  }

runGuardedPoi :: PoiConf -> Shell (Either Line Line)
runGuardedPoi PoiConf {..} =
  inprocWithErr (fromString electronPath) [fromString poiPath] ""

main :: IO ()
main =
  getArgs >>= \case
    "_dev" : args -> mainForDev args
    _ -> mainRun

mainForDev :: [String] -> IO ()
mainForDev _args = do
  pure ()

mainRun :: IO ()
mainRun = do
  RawOption {..} <- execParser pOpts
  -- finding anything prevents further execution
  let psFold = Fold (\acc i -> acc ++ [i]) [] id
  -- check existing instances ...
  [ec0] <- Turtle.fold (fst <$> shellStrict "pgrep 'electron'" "") psFold
  case ec0 of
    ExitFailure _ -> pure ()
    ExitSuccess -> do
      -- TODO: this is quick & dirty
      putStrLn "found electron running, exiting ..."
      exitFailure
  -- verification.
  putStr "Verifying poi directory ... "
  doesDirectoryExist poiPath >>= \case
    True -> putStrLn "yes"
    False -> do
      putStrLn "no"
      putStrLn $ "Cannot find poi directory: " ++ poiPath
      exitFailure

  putStr "Verifying electron executable ... "
  doesFileExist electronPath >>= \case
    True -> putStrLn "yes"
    False -> do
      putStrLn "no"
      putStrLn $ "Cannot find electron executable: " ++ electronPath
      exitFailure
  -- setup environment
  modeStr <- case poiModeRaw of
    "prod" -> pure "production"
    "devel" -> pure "development"
    raw -> do
      putStrLn $ "Found invalid mode: \"" ++ raw ++ "\", fallback to production mode"
      pure "production"
  setEnv "NODE_ENV" modeStr

  setEnv "ELECTRON_ENABLE_LOGGING" "true"
  setEnv "ELECTRON_ENABLE_STACK_DUMPING" "true"

  sysTmpDir <- getTemporaryDirectory
  ts <- getCurTimestamp
  -- directory pattern: poi-launcher-<timestamp>-<random>
  tmpPath <- createTempDirectory sysTmpDir ("poi-launcher-" <> ts)
  -- we'll run poi in tmp dir
  oldDir <- getCurrentDirectory
  setCurrentDirectory tmpPath
  putStrLn $ "Temporary path created at: " <> tmpPath
  let outFile = tmpPath ++ "/stdout"
      errFile = tmpPath ++ "/stderr"
  hOut <- openFile outFile WriteMode
  hErr <- openFile errFile WriteMode
  hSetBuffering hOut LineBuffering
  hSetBuffering hErr LineBuffering
  let progFold :: FoldM IO (Either Line Line) ()
      progFold = FoldM step initial extract
        where
          step _ out = case out of
            Left errOut ->
              T.hPutStrLn hErr (lineToText errOut)
            Right stdOut ->
              T.hPutStrLn hOut (lineToText stdOut)
          initial = pure ()
          extract _ = pure ()
  catch (foldIO (runGuardedPoi (PoiConf {..})) progFold) $ \ec1@(ExitFailure _) -> do
    putStrLn $ "poi failed with exitcode: " ++ show ec1
    putStrLn "Temporary directory is kept for investigation."
    hClose hOut >> hClose hErr
    exit ec1
  setCurrentDirectory oldDir
  -- there seems to be no way of knowing whether electron is killed or terminated normally
  -- so let's always keep logs
  -- putStrLn "Executed successfully, removing temporary directory ..."
  hClose hOut >> hClose hErr
