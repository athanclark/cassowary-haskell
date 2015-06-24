module Paths_cassowary_haskell (
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
version = Version [0,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/athan/dev/cassowary-purescript/cassowary-haskell/.cabal-sandbox/bin"
libdir     = "/home/athan/dev/cassowary-purescript/cassowary-haskell/.cabal-sandbox/lib/x86_64-linux-ghc-7.10.1/casso_6NqdIOrG0p94M3AQPREnCC"
datadir    = "/home/athan/dev/cassowary-purescript/cassowary-haskell/.cabal-sandbox/share/x86_64-linux-ghc-7.10.1/cassowary-haskell-0.0.0"
libexecdir = "/home/athan/dev/cassowary-purescript/cassowary-haskell/.cabal-sandbox/libexec"
sysconfdir = "/home/athan/dev/cassowary-purescript/cassowary-haskell/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "cassowary_haskell_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "cassowary_haskell_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "cassowary_haskell_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cassowary_haskell_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cassowary_haskell_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
