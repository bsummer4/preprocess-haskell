{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}

module Language.Haskell.Preprocess where


-- Imports -------------------------------------------------------------------

import BasicPrelude hiding (empty,find)
import Prelude.Unicode
import Control.Category.Unicode
import Turtle
import qualified Prelude

import qualified Filesystem.Path.CurrentOS as P
import qualified Control.Foldl as Fold
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Maybe

import qualified Language.Preprocessor.Unlit as CPP
import qualified Language.Preprocessor.Cpphs as CPP

import System.Posix.Process
import qualified System.IO.Temp as IO
import qualified System.IO as IO
import Control.DeepSeq

import qualified Distribution.PackageDescription as C
import qualified Distribution.PackageDescription.Parse as C
import qualified Distribution.Verbosity as C
import qualified Distribution.PackageDescription.Configuration as C

import Debug.Trace


-- Types ---------------------------------------------------------------------

newtype ModuleName = MN { unModuleName ∷ Text }
  deriving (Eq,Ord,IsString,Show)

-- | Path's relative to the root of the source tree.
newtype SrcTreePath = STP { unSTP ∷ P.FilePath }
  deriving (Eq,Ord,IsString,Show)

-- | The name and preprocessed contents of a source file.
data Module = Module
  { mFilename ∷ !SrcTreePath
  , mSource ∷ String
  }
  deriving (Show)

-- | The files of a cabal package.
type Package = Map ModuleName Module

-- | Map from a cabal file to it's associated source files.
type SourceTree = Map SrcTreePath Package


-- Values --------------------------------------------------------------------

stpStr ∷ SrcTreePath → String
stpStr = P.encodeString . unSTP

cabalFiles ∷ Pattern Text
cabalFiles = suffix ".cabal"

haskellFiles ∷ Pattern Text
haskellFiles = suffix ".hs" <|> suffix ".lhs"

consFold ∷ Fold a [a]
consFold = Fold.Fold (flip (:)) [] reverse

shellLines ∷ Shell a → IO [a]
shellLines = flip fold consFold

literateHaskellFilename ∷ P.FilePath → Bool
literateHaskellFilename fp = Just "lhs" ≡ P.extension fp

processFile ∷ String → SrcTreePath → IO Module
processFile macros fn = do
  IO.withSystemTempFile "cabal_macros.h" $ \fp handle → do
    IO.hPutStr handle macros
    IO.hPutStr handle "#define __GLASGOW_HASKELL__ 708"
    IO.hClose handle
    let pstr = P.encodeString (unSTP fn)
    contents ← Prelude.readFile pstr
    let defaults = CPP.defaultCpphsOptions
        cppOpts = defaults {
          CPP.preInclude = [fp],
          CPP.boolopts = (CPP.boolopts defaults) {
            CPP.literate = literateHaskellFilename(unSTP fn) }}
    noMacros ← CPP.runCpphs cppOpts pstr contents
    noMacros `deepseq` return(Module fn noMacros)

-- TODO Why doesn't this work?
processFileSane ∷ [(String,String)] → SrcTreePath → IO Module
processFileSane macros fn = do
  let pstr = P.encodeString (unSTP fn)
  contents ← Prelude.readFile pstr
  let defaults = CPP.defaultCpphsOptions
  let cppOpts = defaults {
    CPP.defines = macros ++ CPP.defines defaults,
    CPP.boolopts = (CPP.boolopts defaults) {
      CPP.literate = literateHaskellFilename(unSTP fn) }}
  noMacros ← CPP.runCpphs cppOpts pstr contents
  return $ Module fn noMacros

moduleName ∷ [SrcTreePath] → SrcTreePath → Maybe ModuleName
moduleName srcDirs fn = listToMaybe $ moduleNames
    where tryPrefix = flip P.stripPrefix $ unSTP fn
          pathToModule = P.splitDirectories
                       ⋙ fmap (T.filter (≠'/') . T.pack . P.encodeString)
                       ⋙ T.intercalate "."
                       ⋙ MN
          moduleNames = pathToModule . P.dropExtensions <$> pathNames
          pathNames = catMaybes $ tryPrefix . unSTP <$> srcDirs

-- TODO nub is not your friend.
-- TODO Handle parse failures!
allSourceDirs ∷ C.PackageDescription → [Prelude.FilePath]
allSourceDirs desc = nub $ join $ libDirs ++ exeDirs
  where
     libDirs = maybeToList (C.hsSourceDirs . C.libBuildInfo <$> C.library desc)
     exeDirs = C.hsSourceDirs . C.buildInfo <$> C.executables desc

macroPlaceholder ∷ IO String
macroPlaceholder = do
  f ← Prelude.readFile "/home/ben/preprocess-haskell/dist/build/autogen/cabal_macros.h"
  f `deepseq` return f

  -- contents ← Prelude.readFile fn
  -- snd <$> CPP.runCpphsReturningSymTab CPP.defaultCpphsOptions fn contents

cabalMacros ∷ C.PackageDescription → IO String
cabalMacros desc = macroPlaceholder

cabalInfo ∷ SrcTreePath → IO ([SrcTreePath],String)
cabalInfo cabalFile = do
  -- traceM "cabalSourceDirs"
  gdesc ← C.readPackageDescription C.normal $ stpStr cabalFile
  let desc        = C.flattenPackageDescription gdesc
      pkgRoot     = directory $ unSTP cabalFile
      dirStrs     = allSourceDirs desc
      toSTP d     = STP $ P.collapse $ pkgRoot </> P.decodeString(d <> "/")
  -- traceM $ Prelude.show $ toSTP <$> dirStrs
  macros ← cabalMacros desc
  return (toSTP <$> dirStrs, macros)

processPackage ∷ SrcTreePath → IO Package
processPackage fn = do
  -- traceM $ P.encodeString $ unSTP fn
  (srcDirs,macros) ← cabalInfo fn
  -- traceM $ "srcDirs: " ++ Prelude.show srcDirs
  hsFiles ← fmap STP <$> shellLines (find haskellFiles ".")
  -- traceM $ "hsFiles: " ++ Prelude.show hsFiles
  fmap (M.fromList . catMaybes) $ forM hsFiles $ \hs → do
    let nm = moduleName srcDirs hs
    -- case nm of
      -- Nothing → return()
      -- Just n → traceM $ Prelude.show n ++ "(" ++ Prelude.show(unSTP hs) ++ ")"
    src ← processFile macros hs
    return $ (,src) <$> nm

findPackages ∷ IO [SrcTreePath]
findPackages = fmap STP <$> shellLines (find cabalFiles ".")

processSourceTree ∷ FilePath → IO SourceTree
processSourceTree fp = do
  -- traceM $ "cd " ++ Prelude.show fp
  cd fp
  -- traceM $ "findPackages"
  packages ← findPackages
  -- traceM $ "packages " ++ (Prelude.show packages)
  fmap M.fromList $ forM packages $ \p → do
    result ← processPackage p
    return (p,result)

loc ∷ FilePath → IO Int
loc fp = do
  tree ← processSourceTree fp
  let allCode = join $ (mSource <$> join(M.elems <$> M.elems tree))
  return $ length $ Prelude.lines $ allCode
