{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections #-}

module Language.Haskell.Preprocess.Internal where


-- Imports -------------------------------------------------------------------

import BasicPrelude hiding (Map, empty, find, forM, mapM, show)
import Turtle       hiding (e, fp, root, x)

import           Control.Category.Unicode
import           Debug.Trace
import           Prelude                  (show)
import qualified Prelude
import           Prelude.Unicode
import           Text.Printf


import Language.Haskell.Exts.Annotated as HSE hiding (ModuleName, fileName,
                                               mode)

import           Control.DeepSeq
import qualified Control.Exception
import qualified Control.Foldl             as Fold
import qualified Data.ByteString           as BS
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as M
import qualified Data.Set                  as S
import qualified Data.Text                 as T
import qualified Filesystem.Path.CurrentOS as P

import qualified Language.Preprocessor.Cpphs as CPP

import qualified System.IO.Temp as IO

import qualified Distribution.Package                          as C
import qualified Distribution.PackageDescription               as C
import qualified Distribution.PackageDescription.Configuration as C
import qualified Distribution.PackageDescription.Parse         as C
import qualified Distribution.Simple.Build.Macros              as C
import qualified Distribution.Verbosity                        as C
import qualified Distribution.Version                          as C
import qualified Language.Haskell.Extension                    as C

import Language.Haskell.Preprocess.Macros


-- Types ---------------------------------------------------------------------

newtype ModuleName = MN { unModuleName ∷ Text }
  deriving (Eq,Ord,IsString,Show)

-- | Path's relative to the root of the source tree.
newtype SrcTreePath = SrcTreePath { unSTP ∷ P.FilePath }
  deriving (Eq,Ord,IsString,Show,NFData)

-- TODO Consider writing a newtype for CabalFile
-- TODO Consider writing a newtype for HSFile
-- TODO Consider writing a newtype for SrcDir

data Pkg = Pkg {
  pkgRoot              ∷ !SrcTreePath,
  pkgCabalFile         ∷ !SrcTreePath,
  pkgModules           ∷ !(Map ModuleName SrcTreePath),
  pkgMacros            ∷ !String,
  pkgIncludeDirs       ∷ ![SrcTreePath],
  pkgDefaultExtensions ∷ ![C.Extension]
}


-- Typeclass Instances -------------------------------------------------------

-- TODO This is highly questionable.
instance NFData C.Extension
instance NFData SrcSpanInfo
instance NFData a ⇒ NFData (Module a)


-- Utilities for Dealing with FilePath's -------------------------------------

pathStr ∷ FilePath → String
pathStr = P.encodeString

mkAbsolute ∷ FilePath → IO FilePath
mkAbsolute fp = if P.absolute fp
                  then return fp
                  else do wd ← pwd
                          return $ P.collapse $ wd <> fp

fileP ∷ P.FilePath → P.FilePath
fileP fp = if fp≠P.directory fp then fp else
            error $ printf "%s is not a path to a file." (show fp)

normalizeP ∷ P.FilePath → P.FilePath
normalizeP fp =
  if P.relative fp then P.collapse $ "./" <> fp else
    error $ printf "`stp` called on absolute path: %s" (show fp)

stFile ∷ P.FilePath → SrcTreePath
stFile = SrcTreePath . normalizeP . fileP

stDir ∷ P.FilePath → SrcTreePath
stDir = SrcTreePath . normalizeP . (<> "")

stpStr ∷ SrcTreePath → String
stpStr = pathStr . unSTP


-- Values --------------------------------------------------------------------

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

hackAroundCPPHSFlaws ∷ String → String
hackAroundCPPHSFlaws = T.pack
     ⋙ T.replace "defined(MIN_VERSION_hashable)" "1"
     ⋙ T.replace "defined(MIN_VERSION_integer_gmp)" "1"
     ⋙ T.unpack

prToMaybe ∷ ParseResult a → Maybe a
prToMaybe (ParseOk x) = Just x
prToMaybe (ParseFailed l c) =
  flip trace Nothing $ printf "Parse failed! %s %s" (show l) (show c)

cvtKExt ∷ C.KnownExtension → Maybe KnownExtension
cvtKExt e = case e of
  C.OverlappingInstances → Just OverlappingInstances
  C.UndecidableInstances → Just UndecidableInstances
  C.IncoherentInstances → Just IncoherentInstances
  C.InstanceSigs → Just InstanceSigs
  C.DoRec → Just DoRec
  C.RecursiveDo → Just RecursiveDo
  C.ParallelListComp → Just ParallelListComp
  C.MultiParamTypeClasses → Just MultiParamTypeClasses
  C.MonomorphismRestriction → Just MonomorphismRestriction
  C.FunctionalDependencies → Just FunctionalDependencies
  C.Rank2Types → Just Rank2Types
  C.RankNTypes → Just RankNTypes
  C.PolymorphicComponents → Just PolymorphicComponents
  C.ExistentialQuantification → Just ExistentialQuantification
  C.ScopedTypeVariables → Just ScopedTypeVariables
  C.PatternSignatures → Just PatternSignatures
  C.ImplicitParams → Just ImplicitParams
  C.FlexibleContexts → Just FlexibleContexts
  C.FlexibleInstances → Just FlexibleInstances
  C.EmptyDataDecls → Just EmptyDataDecls
  C.CPP → Just CPP
  C.KindSignatures → Just KindSignatures
  C.BangPatterns → Just BangPatterns
  C.TypeSynonymInstances → Just TypeSynonymInstances
  C.TemplateHaskell → Just TemplateHaskell
  C.ForeignFunctionInterface → Just ForeignFunctionInterface
  C.Arrows → Just Arrows
  C.Generics → Just Generics
  C.ImplicitPrelude → Just ImplicitPrelude
  C.NamedFieldPuns → Just NamedFieldPuns
  C.PatternGuards → Just PatternGuards
  C.GeneralizedNewtypeDeriving → Just GeneralizedNewtypeDeriving
  C.ExtensibleRecords → Just ExtensibleRecords
  C.RestrictedTypeSynonyms → Just RestrictedTypeSynonyms
  C.HereDocuments → Just HereDocuments
  C.MagicHash → Just MagicHash
  C.TypeFamilies → Just TypeFamilies
  C.StandaloneDeriving → Just StandaloneDeriving
  C.UnicodeSyntax → Just UnicodeSyntax
  C.UnliftedFFITypes → Just UnliftedFFITypes
  C.LiberalTypeSynonyms → Just LiberalTypeSynonyms
  C.TypeOperators → Just TypeOperators
  C.ParallelArrays → Just ParallelArrays
  C.RecordWildCards → Just RecordWildCards
  C.RecordPuns → Just RecordPuns
  C.DisambiguateRecordFields → Just DisambiguateRecordFields
  C.OverloadedStrings → Just OverloadedStrings
  C.GADTs → Just GADTs
  C.MonoPatBinds → Just MonoPatBinds
  C.RelaxedPolyRec → Just RelaxedPolyRec
  C.ExtendedDefaultRules → Just ExtendedDefaultRules
  C.UnboxedTuples → Just UnboxedTuples
  C.DeriveDataTypeable → Just DeriveDataTypeable
  C.ConstrainedClassMethods → Just ConstrainedClassMethods
  C.PackageImports → Just PackageImports
  C.LambdaCase → Just LambdaCase
  C.ImpredicativeTypes → Just ImpredicativeTypes
  C.NewQualifiedOperators → Just NewQualifiedOperators
  C.PostfixOperators → Just PostfixOperators
  C.QuasiQuotes → Just QuasiQuotes
  C.TransformListComp → Just TransformListComp
  C.ViewPatterns → Just ViewPatterns
  C.XmlSyntax → Just XmlSyntax
  C.RegularPatterns → Just RegularPatterns
  C.TupleSections → Just TupleSections
  C.GHCForeignImportPrim → Just GHCForeignImportPrim
  C.NPlusKPatterns → Just NPlusKPatterns
  C.DoAndIfThenElse → Just DoAndIfThenElse
  C.RebindableSyntax → Just RebindableSyntax
  C.ExplicitForAll → Just ExplicitForAll
  C.DatatypeContexts → Just DatatypeContexts
  C.MonoLocalBinds → Just MonoLocalBinds
  C.DeriveFunctor → Just DeriveFunctor
  C.DeriveGeneric → Just DeriveGeneric
  C.DeriveTraversable → Just DeriveTraversable
  C.DeriveFoldable → Just DeriveFoldable
  C.NondecreasingIndentation → Just NondecreasingIndentation
  C.InterruptibleFFI → Just InterruptibleFFI
  C.CApiFFI → Just CApiFFI
  C.ExplicitNamespaces → Just ExplicitNamespaces
  C.DataKinds → Just DataKinds
  C.PolyKinds → Just PolyKinds
  C.MultiWayIf → Just MultiWayIf
  C.SafeImports → Just SafeImports
  C.Safe → Just Safe
  C.Trustworthy → Just Trustworthy
  C.DefaultSignatures → Just DefaultSignatures
  C.ConstraintKinds → Just ConstraintKinds
  _ → flip trace Nothing $ printf "Unknown Extension: %s" (show e)

maybeUnknown ∷ C.KnownExtension → Maybe Extension → Extension
maybeUnknown x = fromMaybe $ UnknownExtension $ show x

cvtExt ∷ C.Extension → Extension
cvtExt (C.EnableExtension x)   = maybeUnknown x (EnableExtension  <$> cvtKExt x)
cvtExt (C.DisableExtension x)  = maybeUnknown x (DisableExtension <$> cvtKExt x)
cvtExt (C.UnknownExtension nm) = UnknownExtension nm

extensionsHack ∷ [Extension]
extensionsHack =
  [ EnableExtension NPlusKPatterns
  , EnableExtension ExplicitForAll
  , EnableExtension FlexibleContexts
  , EnableExtension ExplicitNamespaces
  , EnableExtension CPP
  , EnableExtension MagicHash
  , EnableExtension TemplateHaskell
  , EnableExtension MultiParamTypeClasses
  , EnableExtension ExistentialQuantification
  ]

-- | BE CAREFUL! This updates the filesystem at `fp` and depends on autoreconf.
-- TODO write proper code for having a temporary working directory.
configureWithAutotools ∷ FilePath → IO ()
configureWithAutotools fp = do
  wd ← pwd
  cd fp
  traceM "running autoreconf"
  sh $ inshell "autoreconf" empty
  traceM "running ./configure"
  sh $ inshell "./configure" empty
  cd wd

directoriesThatRequireAutotoolsConfiguration ∷ FilePath → IO [FilePath]
directoriesThatRequireAutotoolsConfiguration fp =
  map P.directory <$> shellLines (find (has "configure.ac") fp)

sourceTreeDependsOnAutotools ∷ FilePath → IO Bool
sourceTreeDependsOnAutotools fp = do
  dirs ← directoriesThatRequireAutotoolsConfiguration fp
  return $ 0 ≠ length dirs

configureWithAutotoolsRecursive ∷ FilePath → IO ()
configureWithAutotoolsRecursive fp = do
  dirs ← directoriesThatRequireAutotoolsConfiguration fp
  forM_ dirs configureWithAutotools

analyseCopy ∷ FilePath → (FilePath → IO a) → IO a
analyseCopy fp analyse =
  IO.withSystemTempDirectory "copy_for_analysis" $ \tmpDir → do
   fpstr ← pathStr <$> mkAbsolute fp
   let cmd = printf "(cd '%s'; tar c .) | (cd '%s'; tar x)" fpstr tmpDir
   traceM "copying project to a temporary directory"
   sh $ inshell (T.pack cmd) empty
   traceM "done copying"
   analyse $ P.decodeString tmpDir

analyseConfiguredCopy ∷ FilePath → (FilePath → IO a) → IO a
analyseConfiguredCopy fp analyse =
  analyseCopy fp $ \tmpDir → do
    configureWithAutotoolsRecursive tmpDir
    analyse tmpDir

parseCode ∷ Pkg → FilePath → String → Maybe (Module SrcSpanInfo)
parseCode pkg filePath source = do
  let fileName = pathStr filePath
  let mode = defaultParseMode
        { parseFilename     = fileName
        , fixities          = Nothing
        , ignoreLinePragmas = False
        , extensions        = extensionsHack
                           ++ (cvtExt <$> pkgDefaultExtensions pkg)
                           ++ extensions defaultParseMode
        }
  prToMaybe $ parseFileContentsWithMode mode source

catchAny ∷ IO a → (SomeException → IO a) → IO a
catchAny = Control.Exception.catch

-- TODO This uses deepseq to avoid issues related to lazy IO. I feel
--      like that's a bad idea.
processFile ∷ NFData a ⇒ FilePath → Pkg → (String → a) → SrcTreePath → IO a
processFile root' pkg transform fn = do
  let root = root' <> "" -- Forces root to be a directory path.
  let warn e = do
        traceM $ printf "Ignoring exception: %s" $ show e
        return $ transform ""

  flip catchAny warn $
    IO.withSystemTempDirectory "cpp_macros" $ \tmpDir → do
      let tmpDirFP         = P.decodeString tmpDir
          cabalMacrosFP    = tmpDirFP </> "cabal_macros.h"
          compilerMacrosFP = tmpDirFP </> "ghc_macros.h"

      writeFile cabalMacrosFP    $ T.pack (pkgMacros pkg)
      writeFile compilerMacrosFP $ T.pack compilerMacros
      forM_ (M.toList stdHdrs) $ \(hdrFileName,hdrContents) → do
        let absPath = tmpDirFP </> hdrFileName
        mktree (P.parent absPath)
        BS.writeFile (pathStr absPath) hdrContents

      let pstr = pathStr (P.collapse $ root </> unSTP fn)
      contents ← hackAroundCPPHSFlaws <$> Prelude.readFile pstr

      let pkgIncludes =
            pathStr . P.collapse . (root </>) . unSTP
              <$> pkgIncludeDirs pkg

      let defaults = CPP.defaultCpphsOptions
          cppOpts = defaults {
            CPP.preInclude = pathStr <$> [compilerMacrosFP, cabalMacrosFP],
            CPP.includes = pkgIncludes ++ [tmpDir],
            CPP.boolopts = (CPP.boolopts defaults) {
              CPP.hashline = False,
              CPP.stripC89 = True,
              CPP.warnings = True,
              CPP.literate = literateHaskellFilename(unSTP fn) }}
      noMacros ← CPP.runCpphs cppOpts pstr contents
      let result = transform noMacros
      result `deepseq` return result

moduleName ∷ [SrcTreePath] → SrcTreePath → Maybe ModuleName
moduleName srcDirs fn = listToMaybe moduleNames
    where tryPrefix = flip P.stripPrefix $ unSTP fn
          pathToModule = P.splitDirectories
                       ⋙ fmap (T.filter (≠'/') . T.pack . pathStr)
                       ⋙ T.intercalate "."
                       ⋙ MN
          moduleNames = pathToModule . P.dropExtensions <$> pathNames
          pathNames = catMaybes $ tryPrefix . unSTP <$> srcDirs

-- TODO This is terrible. Rewrite this.
allSourceDirs ∷ C.PackageDescription → [String]
allSourceDirs desc = nub $ join $ libDirs ++ exeDirs
  where
     libDirs = maybeToList (C.hsSourceDirs . C.libBuildInfo <$> C.library desc)
     exeDirs = C.hsSourceDirs . C.buildInfo <$> C.executables desc

-- TODO This is terrible. Rewrite this.
allHeaderIncludeDirs ∷ C.PackageDescription → [String]
allHeaderIncludeDirs desc = nub $ join $ libDirs ++ exeDirs
  where
     libDirs = maybeToList (C.includeDirs . C.libBuildInfo <$> C.library desc)
     exeDirs = C.includeDirs . C.buildInfo <$> C.executables desc

-- TODO This is terrible. Rewrite this.
allDefaultExtensions ∷ C.PackageDescription → [C.Extension]
allDefaultExtensions desc = nub $ join $ libDirs ++ exeDirs
  where
     libDirs = maybeToList (C.defaultExtensions . C.libBuildInfo <$> C.library desc)
     exeDirs = C.defaultExtensions . C.buildInfo <$> C.executables desc

-- `chooseVersion` chooses the greatest version that is explicitly
-- mentioned, but uses 0.1.0.0 if none is mentioned.
chooseVersion ∷ C.VersionRange → C.Version
chooseVersion = C.foldVersionRange fallback id id id max max
  where fallback = C.Version [0,1,0,0] []

pkgDeps ∷ C.GenericPackageDescription → [C.Dependency]
pkgDeps = C.buildDepends . C.flattenPackageDescription

cabalMacros ∷ C.GenericPackageDescription → String
cabalMacros = C.generatePackageVersionMacros . pkgs
  where resolve (C.Dependency n v) = C.PackageIdentifier n $ chooseVersion v
        pkgs = fmap resolve . pkgDeps

cabalInfo ∷ FilePath → SrcTreePath → IO ([SrcTreePath],String,[SrcTreePath],[C.Extension])
cabalInfo root' cabalFile = do
  let root = root' <> "" -- Forces root to be a directory path.
  traceM $ "scanning pakage: " <> stpStr cabalFile
  let cabalFileStr = pathStr $ root <> unSTP cabalFile
  gdesc ← C.readPackageDescription C.normal cabalFileStr
  let desc        = C.flattenPackageDescription gdesc
      rootOfPkg   = directory $ unSTP cabalFile
      dirStrs     = allSourceDirs desc
      incDirs     = allHeaderIncludeDirs desc
      exts        = allDefaultExtensions desc
      toSTP       = stDir . (rootOfPkg <>) . P.decodeString
      macros      = cabalMacros gdesc

  let result = (toSTP <$> dirStrs, macros, toSTP <$> incDirs, exts)
  result `deepseq` return result

scanPkg ∷ FilePath → SrcTreePath → IO Pkg
scanPkg root' cabalFile = do
  let root = root' <> "" -- Forces root to be a directory path.
  let pkgDir = stDir $ P.directory $ unSTP cabalFile

  hsFiles' ← shellLines (find haskellFiles $ root <> unSTP pkgDir)
  (srcDirs,macros,includeDirs,defaultExtensions) ← cabalInfo root cabalFile

  let hsFiles = flip map hsFiles' $ \x →
        let dieWithError = error $ printf "%s is not a prefix of %s!"
                             (pathStr root) (pathStr x)
        in stFile $ fromMaybe dieWithError $ P.stripPrefix root $ P.collapse x
      modules = M.fromList $ catMaybes $ flip map hsFiles $ \hs →
        (,hs) <$> moduleName srcDirs hs

  return $ Pkg pkgDir cabalFile modules macros includeDirs defaultExtensions

scan ∷ FilePath → IO (Set SrcTreePath)
scan root' = do
  let root = root' <> "" -- Forces root to be a directory path.
  packageFiles ← shellLines $ find cabalFiles root
  return $ S.fromList $ flip map packageFiles $ \x →
    let dieWithError = error $ printf "%s is not a prefix of %s!"
                         (pathStr root) (pathStr x)
    in stFile $ fromMaybe dieWithError $ P.stripPrefix root $ P.collapse x
