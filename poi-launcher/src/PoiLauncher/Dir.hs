module PoiLauncher.Dir
  ( experiment
  )
where

import Control.Monad
import Data.Time.Format
  ( defaultTimeLocale
  , formatTime
  )
import Data.Time.LocalTime (getZonedTime)
import System.Directory
  ( createDirectory
  , createDirectoryIfMissing
  , doesDirectoryExist
  , getTemporaryDirectory
  )
import System.FilePath.Posix ((</>))
import System.Posix.Files
  ( directoryMode
  , fileMode
  , getFileStatus
  , setFileMode
  )
import System.Posix.User (getLoginName)

{-
  TODO:

  Deal with creating directory and stuff here:

  - check if /tmp/poi-launcher-{user} exists
  - ensure that we got 700 on directory
  - create /tmp/poi-launcher-{user}/YYYYMMDD_HHMMSS/
    and use that as current directory.
    (abort if already exist)

 -}

{-

  The container dir is /tmp/poi-launcher-{user}, which
  is then used to contain all logs in its subdirectories.

  - if not exist, create with 700.
  - if exist, ensure mod is 700.

  return directory path.

 -}
ensureContainerDir :: IO FilePath
ensureContainerDir = do
  t <- getTemporaryDirectory
  u <- getLoginName
  let cDir = t </> "poi-launcher-" <> u
  createDirectoryIfMissing False cDir
  st <- getFileStatus cDir
  let expectedMode = directoryMode + 0o700
  unless (fileMode st == expectedMode) do
    setFileMode cDir expectedMode
  pure cDir

ensureWorkingDir :: FilePath -> IO FilePath
ensureWorkingDir cDir = do
  zt <- getZonedTime
  let dName = formatTime defaultTimeLocale "%0Y%m%d_%H%M%S" zt
      dir = cDir </> dName
  e <- doesDirectoryExist dir
  when e do
    error $ "Directory already exist: " <> dir
  dir <$ createDirectory dir

experiment :: IO ()
experiment = do
  z <- ensureContainerDir >>= ensureWorkingDir
  print z
