{-# LANGUAGE RecordWildCards #-}
module Main where

import Options.Applicative
import System.IO.Temp (createTempDirectory)
import System.Directory
import Data.Time

data PoiMode = Prod | Devel

data RawOption = RawOption
  { poiPath :: FilePath
  , electronPath :: FilePath
  , poiModeRaw :: String
  }

pOpts :: ParserInfo RawOption
pOpts = info (rawOpts <**> helper)
    (  fullDesc
    <> header "sisyphos: a poi launcher"
    )
  where
    rawOpts :: Parser RawOption
    rawOpts = RawOption
      <$> strOption
          (  long "poi-path"
          <> metavar "PATH"
          <> help "poi app path"
          )
      <*> strOption
          (  long "electron-path"
          <> metavar "PATH"
          <> help "electron bin path"
          )
      <*> option auto
          (  long "mode"
          <> metavar "MODE"
          <> showDefault
          <> value "prod"
          <> help "poi launch mode, must be either \"prod\" or \"devel\""
          )

getCurTimestamp :: IO String
getCurTimestamp =
  formatTime defaultTimeLocale "%_Y%m%d%H%M%S" <$> getZonedTime

main :: IO ()
main = do
  RawOption {..} <- execParser pOpts
  tmpDir <- getTemporaryDirectory
  ts <- getCurTimestamp
  tmpPath <- createTempDirectory tmpDir ("sisyphos-" <> ts)
  
  putStrLn $ "Temp Path created at: " <> tmpPath
  -- TODO
  pure ()
