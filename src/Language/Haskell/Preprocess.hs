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

import qualified Distribution.PackageDescription as C
import qualified Distribution.PackageDescription.Parse as C
import qualified Distribution.Verbosity as C
import qualified Distribution.PackageDescription.Configuration as C

import Debug.Trace


-- Types ---------------------------------------------------------------------

newtype ModuleName = MN { unModuleName ∷ Text }
  deriving (Eq,Ord,IsString,Show)

-- | Path's relative to the root of the source tree.
newtype SourceTreePath = STP { unSTP ∷ P.FilePath }
  deriving (Eq,Ord,IsString,Show)

-- | The name and preprocessed contents of a source file.
data Module = Module
  { mFilename ∷ !SourceTreePath
  , mSource ∷ String
  }
  deriving (Show)

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
  return $ Module fn contents
  noMacros ← CPP.runCpphs CPP.defaultCpphsOptions pstr contents
  return $ Module fn $ if literateHaskellFilename (unSTP fn)
    then CPP.unlit pstr noMacros
    else noMacros

unEither ∷ Either a a → a
unEither (Left a) = a
unEither (Right a) = a

moduleName ∷ [SourceTreePath] → SourceTreePath → Maybe ModuleName
moduleName srcDirs fn = listToMaybe $ moduleNames
    where tryPrefix = flip P.stripPrefix $ unSTP fn
          pathToModule = P.splitDirectories
                       ⋙ fmap (T.filter (≠'/') . unEither . P.toText)
                       ⋙ T.intercalate "."
                       ⋙ MN
          toRelDir d = P.decodeString $ "./" <> P.encodeString d <> "/"
          moduleNames = pathToModule . P.dropExtensions <$> pathNames
          pathNames = catMaybes $ tryPrefix . toRelDir . unSTP <$> srcDirs

-- TODO nub is not your friend.
-- TODO Handle parse failures!
allSourceDirs ∷ C.PackageDescription → [Prelude.FilePath]
allSourceDirs desc = nub $ join $ libDirs ++ exeDirs
  where
     libDirs = maybeToList (C.hsSourceDirs . C.libBuildInfo <$> C.library desc)
     exeDirs = C.hsSourceDirs . C.buildInfo <$> C.executables desc

cabalSourceDirs ∷ SourceTreePath → IO [SourceTreePath]
cabalSourceDirs cabalFile = do
  traceM "cabalSourceDirs"
  gdesc ← C.readPackageDescription C.normal $ stpStr cabalFile
  let dirStrs = allSourceDirs $ C.flattenPackageDescription gdesc
  traceM $ Prelude.show $ STP . P.decodeString <$> dirStrs
  return $ STP . P.decodeString <$> dirStrs

processPackage ∷ SourceTreePath → IO Package
processPackage fn@(STP p) = do
  traceM "processPackage"
  srcDirs ← cabalSourceDirs fn
  traceM $ "srcDirs: " ++ Prelude.show srcDirs
  hsFiles ← fmap STP <$> shellLines (find haskellFiles ".")
  traceM $ "hsFiles: " ++ Prelude.show hsFiles
  fmap (M.fromList . catMaybes) $ forM hsFiles $ \hs → do
    traceM $ "hs: " ++ Prelude.show hs
    let nm = moduleName srcDirs hs
    traceM $ "nm: " ++ Prelude.show nm
    src ← processFile hs
    return $ (,src) <$> nm

findPackages ∷ IO [SourceTreePath]
findPackages = fmap STP <$> shellLines (find cabalFiles ".")

processSourceTree ∷ FilePath → IO SourceTree
processSourceTree fp = do
  traceM $ "cd " ++ Prelude.show fp
  cd fp
  traceM $ "findPackages"
  packages ← findPackages
  traceM $ "packages " ++ (Prelude.show packages)
  fmap M.fromList $ forM packages $ \p → do
    result ← processPackage p
    return (p,result)

loc ∷ FilePath → IO Int
loc fp = do
  tree ← processSourceTree fp
  let allCode = join $ (mSource <$> join(M.elems <$> M.elems tree))
  return $ length $ Prelude.lines $ allCode
