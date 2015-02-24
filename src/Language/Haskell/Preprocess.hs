{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Haskell.Preprocess where


-- Imports -------------------------------------------------------------------

import BasicPrelude hiding (empty,find)
import Prelude.Unicode
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


-- Types ---------------------------------------------------------------------

newtype ModuleName = MN { unModuleName ∷ Text }
  deriving (Eq,Ord,IsString)

-- | Path's relative to the root of the source tree.
newtype SourceTreePath = STP { unSTP ∷ P.FilePath }
  deriving (Eq,Ord,IsString)

-- | The name and preprocessed contents of a source file.
data Module = Module
  { mFilename ∷ !SourceTreePath
  , mSource ∷ String
  }

-- | The files of a cabal package.
type Package = Map ModuleName Module

-- | Map from a cabal file to it's associated source files.
type SourceTree = Map SourceTreePath Package


-- Values --------------------------------------------------------------------

stpStr ∷ SourceTreePath → String
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

processFile ∷ SourceTreePath → IO Module
processFile fn = do
  let pstr = P.encodeString (unSTP fn)
  contents ← Prelude.readFile pstr
  noMacros ← CPP.runCpphs CPP.defaultCpphsOptions pstr contents
  return $ Module fn $ if literateHaskellFilename (unSTP fn)
    then CPP.unlit pstr noMacros
    else noMacros

moduleName ∷ [SourceTreePath] → SourceTreePath → Maybe ModuleName
moduleName srcDirs fn = listToMaybe $ moduleNames
    where tryPrefix = flip P.stripPrefix $ unSTP fn
          pathToModule = P.splitDirectories
                       ⋙ fmap (T.pack . P.encodeString)
                       ⋙ T.intercalate "."
                       ⋙ MN
          moduleNames = pathToModule . P.dropExtensions <$> pathNames
          pathNames = catMaybes $ tryPrefix . unSTP <$> srcDirs

cabalSourceDirs ∷ SourceTreePath → IO [SourceTreePath]
cabalSourceDirs cabalFile = do
  contents ← Prelude.readFile $ stpStr cabalFile
  -- TODO Parse the cabal file, and extract a list of source directories.
  return ["."]

processPackage ∷ SourceTreePath → IO Package
processPackage fn@(STP p) = do
  srcDirs ← cabalSourceDirs fn
  hsFiles ← fmap STP <$> shellLines (find haskellFiles p)
  fmap M.fromList $ forM hsFiles $ \hs → do
    let nm = fromMaybe "Unknown" $ moduleName srcDirs fn
    src ← processFile hs
    return (nm,src)

findPackages ∷ IO [SourceTreePath]
findPackages = fmap STP <$> shellLines (find cabalFiles ".")

processSourceTree ∷ FilePath → IO SourceTree
processSourceTree fp = do
  cd fp
  packages ← findPackages
  fmap M.fromList $ forM packages $ \p → do
    result ← processPackage p
    return (p,result)
