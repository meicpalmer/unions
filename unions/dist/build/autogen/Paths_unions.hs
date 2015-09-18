module Paths_unions (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,0,1], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/michael/.cabal/bin"
libdir     = "/home/michael/.cabal/lib/x86_64-linux-ghc-7.8.4/unions-0.0.1"
datadir    = "/home/michael/.cabal/share/x86_64-linux-ghc-7.8.4/unions-0.0.1"
libexecdir = "/home/michael/.cabal/libexec"
sysconfdir = "/home/michael/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "unions_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "unions_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "unions_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "unions_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "unions_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
