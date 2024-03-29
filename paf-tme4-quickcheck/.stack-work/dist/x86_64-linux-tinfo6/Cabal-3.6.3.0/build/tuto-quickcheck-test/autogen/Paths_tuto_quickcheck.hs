{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_tuto_quickcheck (
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
bindir     = "/home/s-daniiel/Desktop/M1 STL 2022-2023/Semestre 2/PAF - Programmation Avanc\233e en Fonctionnel/Sorbonne_PAF/paf-tme4-quickcheck/.stack-work/install/x86_64-linux-tinfo6/ce71525328c665bc7303fb15c6cfc54c99c15444e26e5d903598efdd8cbcd573/9.2.5/bin"
libdir     = "/home/s-daniiel/Desktop/M1 STL 2022-2023/Semestre 2/PAF - Programmation Avanc\233e en Fonctionnel/Sorbonne_PAF/paf-tme4-quickcheck/.stack-work/install/x86_64-linux-tinfo6/ce71525328c665bc7303fb15c6cfc54c99c15444e26e5d903598efdd8cbcd573/9.2.5/lib/x86_64-linux-ghc-9.2.5/tuto-quickcheck-0.1.0.0-FtkAnzqsmTnDaaOBSnHVQH-tuto-quickcheck-test"
dynlibdir  = "/home/s-daniiel/Desktop/M1 STL 2022-2023/Semestre 2/PAF - Programmation Avanc\233e en Fonctionnel/Sorbonne_PAF/paf-tme4-quickcheck/.stack-work/install/x86_64-linux-tinfo6/ce71525328c665bc7303fb15c6cfc54c99c15444e26e5d903598efdd8cbcd573/9.2.5/lib/x86_64-linux-ghc-9.2.5"
datadir    = "/home/s-daniiel/Desktop/M1 STL 2022-2023/Semestre 2/PAF - Programmation Avanc\233e en Fonctionnel/Sorbonne_PAF/paf-tme4-quickcheck/.stack-work/install/x86_64-linux-tinfo6/ce71525328c665bc7303fb15c6cfc54c99c15444e26e5d903598efdd8cbcd573/9.2.5/share/x86_64-linux-ghc-9.2.5/tuto-quickcheck-0.1.0.0"
libexecdir = "/home/s-daniiel/Desktop/M1 STL 2022-2023/Semestre 2/PAF - Programmation Avanc\233e en Fonctionnel/Sorbonne_PAF/paf-tme4-quickcheck/.stack-work/install/x86_64-linux-tinfo6/ce71525328c665bc7303fb15c6cfc54c99c15444e26e5d903598efdd8cbcd573/9.2.5/libexec/x86_64-linux-ghc-9.2.5/tuto-quickcheck-0.1.0.0"
sysconfdir = "/home/s-daniiel/Desktop/M1 STL 2022-2023/Semestre 2/PAF - Programmation Avanc\233e en Fonctionnel/Sorbonne_PAF/paf-tme4-quickcheck/.stack-work/install/x86_64-linux-tinfo6/ce71525328c665bc7303fb15c6cfc54c99c15444e26e5d903598efdd8cbcd573/9.2.5/etc"

getBinDir     = catchIO (getEnv "tuto_quickcheck_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "tuto_quickcheck_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "tuto_quickcheck_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "tuto_quickcheck_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "tuto_quickcheck_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "tuto_quickcheck_sysconfdir") (\_ -> return sysconfdir)




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
