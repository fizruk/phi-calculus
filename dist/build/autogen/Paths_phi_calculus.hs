{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_phi_calculus (
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

bindir     = "/home/danila/.cabal/bin"
libdir     = "/home/danila/.cabal/lib/x86_64-linux-ghc-8.6.5/phi-calculus-0.1.0.0-HPOsmtE45ThL8va2Lz6tFR"
dynlibdir  = "/home/danila/.cabal/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/danila/.cabal/share/x86_64-linux-ghc-8.6.5/phi-calculus-0.1.0.0"
libexecdir = "/home/danila/.cabal/libexec/x86_64-linux-ghc-8.6.5/phi-calculus-0.1.0.0"
sysconfdir = "/home/danila/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "phi_calculus_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "phi_calculus_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "phi_calculus_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "phi_calculus_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "phi_calculus_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "phi_calculus_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
