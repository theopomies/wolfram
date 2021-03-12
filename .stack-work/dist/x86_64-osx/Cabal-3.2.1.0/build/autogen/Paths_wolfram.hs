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

bindir     = "/Users/kkakura/Epitech/delivery/tek2/B-FUN-400/B-FUN-400-BDX-4-1-wolfram-theo.pomies/.stack-work/install/x86_64-osx/06d144690800509f008e7c884af8d37347a52d8c6ee424fffc0fc6bbde15d79b/8.10.4/bin"
libdir     = "/Users/kkakura/Epitech/delivery/tek2/B-FUN-400/B-FUN-400-BDX-4-1-wolfram-theo.pomies/.stack-work/install/x86_64-osx/06d144690800509f008e7c884af8d37347a52d8c6ee424fffc0fc6bbde15d79b/8.10.4/lib/x86_64-osx-ghc-8.10.4/wolfram-0.1.0.0-ARCiN1jnI0dHEtgtfDeLuQ"
dynlibdir  = "/Users/kkakura/Epitech/delivery/tek2/B-FUN-400/B-FUN-400-BDX-4-1-wolfram-theo.pomies/.stack-work/install/x86_64-osx/06d144690800509f008e7c884af8d37347a52d8c6ee424fffc0fc6bbde15d79b/8.10.4/lib/x86_64-osx-ghc-8.10.4"
datadir    = "/Users/kkakura/Epitech/delivery/tek2/B-FUN-400/B-FUN-400-BDX-4-1-wolfram-theo.pomies/.stack-work/install/x86_64-osx/06d144690800509f008e7c884af8d37347a52d8c6ee424fffc0fc6bbde15d79b/8.10.4/share/x86_64-osx-ghc-8.10.4/wolfram-0.1.0.0"
libexecdir = "/Users/kkakura/Epitech/delivery/tek2/B-FUN-400/B-FUN-400-BDX-4-1-wolfram-theo.pomies/.stack-work/install/x86_64-osx/06d144690800509f008e7c884af8d37347a52d8c6ee424fffc0fc6bbde15d79b/8.10.4/libexec/x86_64-osx-ghc-8.10.4/wolfram-0.1.0.0"
sysconfdir = "/Users/kkakura/Epitech/delivery/tek2/B-FUN-400/B-FUN-400-BDX-4-1-wolfram-theo.pomies/.stack-work/install/x86_64-osx/06d144690800509f008e7c884af8d37347a52d8c6ee424fffc0fc6bbde15d79b/8.10.4/etc"

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
