{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_hw1 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/kanopus/Haskell/homework-1-KanopusSTR/.stack-work/install/x86_64-linux/462ef9aa7004dc5e011dc0b9ddc857b1f5b90889e6583ce3e99a27ac5cecf4c8/9.4.7/bin"
libdir     = "/home/kanopus/Haskell/homework-1-KanopusSTR/.stack-work/install/x86_64-linux/462ef9aa7004dc5e011dc0b9ddc857b1f5b90889e6583ce3e99a27ac5cecf4c8/9.4.7/lib/x86_64-linux-ghc-9.4.7/hw1-0.1.0.0-1KPC1mnL8b3EXz4i819riF-hw1-exe"
dynlibdir  = "/home/kanopus/Haskell/homework-1-KanopusSTR/.stack-work/install/x86_64-linux/462ef9aa7004dc5e011dc0b9ddc857b1f5b90889e6583ce3e99a27ac5cecf4c8/9.4.7/lib/x86_64-linux-ghc-9.4.7"
datadir    = "/home/kanopus/Haskell/homework-1-KanopusSTR/.stack-work/install/x86_64-linux/462ef9aa7004dc5e011dc0b9ddc857b1f5b90889e6583ce3e99a27ac5cecf4c8/9.4.7/share/x86_64-linux-ghc-9.4.7/hw1-0.1.0.0"
libexecdir = "/home/kanopus/Haskell/homework-1-KanopusSTR/.stack-work/install/x86_64-linux/462ef9aa7004dc5e011dc0b9ddc857b1f5b90889e6583ce3e99a27ac5cecf4c8/9.4.7/libexec/x86_64-linux-ghc-9.4.7/hw1-0.1.0.0"
sysconfdir = "/home/kanopus/Haskell/homework-1-KanopusSTR/.stack-work/install/x86_64-linux/462ef9aa7004dc5e011dc0b9ddc857b1f5b90889e6583ce3e99a27ac5cecf4c8/9.4.7/etc"

getBinDir     = catchIO (getEnv "hw1_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "hw1_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "hw1_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "hw1_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hw1_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hw1_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
