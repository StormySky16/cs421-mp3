{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_mp3_cps (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\drago\\Documents\\GitHub\\cs421-mp3\\mps\\mp3-cps\\.stack-work\\install\\3160b525\\bin"
libdir     = "C:\\Users\\drago\\Documents\\GitHub\\cs421-mp3\\mps\\mp3-cps\\.stack-work\\install\\3160b525\\lib\\x86_64-windows-ghc-8.10.7\\mp3-cps-0.1.0.0-IIgsxewe35z5rTdlPxOcYZ-test"
dynlibdir  = "C:\\Users\\drago\\Documents\\GitHub\\cs421-mp3\\mps\\mp3-cps\\.stack-work\\install\\3160b525\\lib\\x86_64-windows-ghc-8.10.7"
datadir    = "C:\\Users\\drago\\Documents\\GitHub\\cs421-mp3\\mps\\mp3-cps\\.stack-work\\install\\3160b525\\share\\x86_64-windows-ghc-8.10.7\\mp3-cps-0.1.0.0"
libexecdir = "C:\\Users\\drago\\Documents\\GitHub\\cs421-mp3\\mps\\mp3-cps\\.stack-work\\install\\3160b525\\libexec\\x86_64-windows-ghc-8.10.7\\mp3-cps-0.1.0.0"
sysconfdir = "C:\\Users\\drago\\Documents\\GitHub\\cs421-mp3\\mps\\mp3-cps\\.stack-work\\install\\3160b525\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "mp3_cps_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "mp3_cps_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "mp3_cps_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "mp3_cps_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "mp3_cps_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "mp3_cps_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
