{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_wolfram (
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

bindir     = "/Users/kkakura/Epitech/delivery/tek2/B-FUN-400/B-FUN-400-BDX-4-1-wolfram-theo.pomies/.stack-work/install/x86_64-osx/479b19087cd05581f435017042e0d7a3529a184b72aa6d74fc2041e80c28b935/8.8.4/bin"
libdir     = "/Users/kkakura/Epitech/delivery/tek2/B-FUN-400/B-FUN-400-BDX-4-1-wolfram-theo.pomies/.stack-work/install/x86_64-osx/479b19087cd05581f435017042e0d7a3529a184b72aa6d74fc2041e80c28b935/8.8.4/lib/x86_64-osx-ghc-8.8.4/wolfram-0.1.0.0-Esipw5UZ0pXIOBMA7X18dP-wolfram-exe"
dynlibdir  = "/Users/kkakura/Epitech/delivery/tek2/B-FUN-400/B-FUN-400-BDX-4-1-wolfram-theo.pomies/.stack-work/install/x86_64-osx/479b19087cd05581f435017042e0d7a3529a184b72aa6d74fc2041e80c28b935/8.8.4/lib/x86_64-osx-ghc-8.8.4"
datadir    = "/Users/kkakura/Epitech/delivery/tek2/B-FUN-400/B-FUN-400-BDX-4-1-wolfram-theo.pomies/.stack-work/install/x86_64-osx/479b19087cd05581f435017042e0d7a3529a184b72aa6d74fc2041e80c28b935/8.8.4/share/x86_64-osx-ghc-8.8.4/wolfram-0.1.0.0"
libexecdir = "/Users/kkakura/Epitech/delivery/tek2/B-FUN-400/B-FUN-400-BDX-4-1-wolfram-theo.pomies/.stack-work/install/x86_64-osx/479b19087cd05581f435017042e0d7a3529a184b72aa6d74fc2041e80c28b935/8.8.4/libexec/x86_64-osx-ghc-8.8.4/wolfram-0.1.0.0"
sysconfdir = "/Users/kkakura/Epitech/delivery/tek2/B-FUN-400/B-FUN-400-BDX-4-1-wolfram-theo.pomies/.stack-work/install/x86_64-osx/479b19087cd05581f435017042e0d7a3529a184b72aa6d74fc2041e80c28b935/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "wolfram_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "wolfram_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "wolfram_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "wolfram_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "wolfram_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "wolfram_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
