{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_hw1 (
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

bindir     = "/home/kanopus/Haskell/homework-1-KanopusSTR/hw1/.stack-work/install/x86_64-linux/5814b5dcc1c4bc631db1b356dcf60a7990aee56a3e974c64d946a87318488a04/8.10.7/bin"
libdir     = "/home/kanopus/Haskell/homework-1-KanopusSTR/hw1/.stack-work/install/x86_64-linux/5814b5dcc1c4bc631db1b356dcf60a7990aee56a3e974c64d946a87318488a04/8.10.7/lib/x86_64-linux-ghc-8.10.7/hw1-0.1.0.0-B8eZHvgbLyOL8zbUqQ6FkT-hw1-exe"
dynlibdir  = "/home/kanopus/Haskell/homework-1-KanopusSTR/hw1/.stack-work/install/x86_64-linux/5814b5dcc1c4bc631db1b356dcf60a7990aee56a3e974c64d946a87318488a04/8.10.7/lib/x86_64-linux-ghc-8.10.7"
datadir    = "/home/kanopus/Haskell/homework-1-KanopusSTR/hw1/.stack-work/install/x86_64-linux/5814b5dcc1c4bc631db1b356dcf60a7990aee56a3e974c64d946a87318488a04/8.10.7/share/x86_64-linux-ghc-8.10.7/hw1-0.1.0.0"
libexecdir = "/home/kanopus/Haskell/homework-1-KanopusSTR/hw1/.stack-work/install/x86_64-linux/5814b5dcc1c4bc631db1b356dcf60a7990aee56a3e974c64d946a87318488a04/8.10.7/libexec/x86_64-linux-ghc-8.10.7/hw1-0.1.0.0"
sysconfdir = "/home/kanopus/Haskell/homework-1-KanopusSTR/hw1/.stack-work/install/x86_64-linux/5814b5dcc1c4bc631db1b356dcf60a7990aee56a3e974c64d946a87318488a04/8.10.7/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hw1_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hw1_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hw1_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hw1_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hw1_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hw1_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
