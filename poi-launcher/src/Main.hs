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
main = do
  RawOption {..} <- execParser pOpts
  -- finding anything prevents further execution
  let psFold = Fold (\acc i -> acc ++ [i]) [] id
  -- check existing instances ...
  [ec] <- fold (fst <$> shellStrict "pgrep 'electron'" "") psFold
  case ec of
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
  catch (foldIO (runGuardedPoi (PoiConf {..})) progFold) $ \ec@(ExitFailure _) -> do
    putStrLn $ "poi failed with exitcode: " ++ show ec
    putStrLn "Temporary directory is kept for investigation."
    hClose hOut >> hClose hErr
    exit ec
  setCurrentDirectory oldDir
  -- there seems to be no way of knowing whether electron is killed or terminated normally
  -- so let's always keep logs
  -- putStrLn "Executed successfully, removing temporary directory ..."
  hClose hOut >> hClose hErr
