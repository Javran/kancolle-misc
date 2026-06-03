module PoiLauncher.Dir
  ( ensureWorkingDir
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
import System.IO.Error (catchIOError)
import System.Posix.Files
  ( createSymbolicLink
  , directoryMode
  , fileMode
  , getFileStatus
  , removeLink
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

ensureWorkingDirAux :: FilePath -> IO FilePath
ensureWorkingDirAux cDir = do
  zt <- getZonedTime
  let
    dName = formatTime defaultTimeLocale "%0Y%m%d_%H%M%S" zt
    dir = cDir </> dName
    linkPath = cDir </> "latest"
  e <- doesDirectoryExist dir
  when e do
    error $ "Directory already exist: " <> dir
  createDirectory dir

  -- create a "latest" link
  catchIOError (removeLink linkPath) (\_ -> pure ())
  createSymbolicLink dName linkPath

  pure dir

ensureWorkingDir :: IO FilePath
ensureWorkingDir = ensureContainerDir >>= ensureWorkingDirAux
