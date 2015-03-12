module Language.Haskell.Preprocess (
 ModuleName(..), SrcTreePath, Pkg,
 unSTP,stFile,stDir,stpStr,
 pkgRoot,pkgCabalFile,pkgModules,pkgMacros,pkgIncludeDirs,pkgDefaultExtensions,
 directoriesThatRequireAutotoolsConfiguration,
 configureWithAutotools,configureWithAutotoolsRecursive,
 analyseCopy,analyseConfiguredCopy,
 scan,
 scanPkg,
 processFile,
 parseCode)
where

import Language.Haskell.Preprocess.Internal
