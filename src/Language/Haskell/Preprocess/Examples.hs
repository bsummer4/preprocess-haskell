module Language.Haskell.Preprocess.Examples where


import BasicPrelude hiding (forM, forM_, mapM, mapM_, sum)

import qualified Prelude

import Control.DeepSeq
import Data.Foldable
import Data.Map.Strict  as M
import Data.Set         as S
import Data.Traversable

import Language.Haskell.Exts.Annotated as HSE hiding (ModuleName)

import Language.Haskell.Preprocess


type EntireProject a = Map SrcTreePath (Pkg, Map ModuleName a)
type MaybeModule = Maybe(Module SrcSpanInfo)

-- | TODO This is poorly thought out.
type Transform a = Pkg → FilePath → String → a

loadPkg ∷ NFData a ⇒
            FilePath → Transform a → SrcTreePath →
              IO (Pkg,Map ModuleName a)
loadPkg root transform pkgFile = do
  pkg ← scanPkg root pkgFile
  sources ← forM (pkgModules pkg) $ \fp →
    processFile root pkg (transform pkg $ unSTP fp) fp
  return (pkg,sources)

loadEntireProject ∷ NFData a ⇒ FilePath → Transform a → IO (EntireProject a)
loadEntireProject root transform = do
  pkgFiles ← S.toList <$> scan root
  mapM (loadPkg root transform) $ M.fromList $ (\x→(x,x)) <$> pkgFiles

parseEntireProject ∷ FilePath → IO (EntireProject MaybeModule)
parseEntireProject root = do
  pkgFiles ← S.toList <$> scan root
  mapM (loadPkg root parseCode) $ M.fromList $ (\x→(x,x)) <$> pkgFiles

listFiles ∷ FilePath → IO [FilePath]
listFiles root = do
  pkgFiles ← S.toList <$> scan root
  fmap (fmap unSTP . join) $ forM pkgFiles $ \pkgFile →
    M.elems . pkgModules <$> scanPkg root pkgFile

loc ∷ FilePath → IO Int
loc root = do
  pkgFiles ← S.toList <$> scan root
  fmap sum $ forM pkgFiles $ \pkgFile → do
    pkg ← scanPkg root pkgFile
    sum <$> forM (M.elems (pkgModules pkg))
              (processFile root pkg (length . Prelude.lines))

configureAndParseEntireProject ∷ FilePath → IO (EntireProject MaybeModule)
configureAndParseEntireProject = flip analyseConfiguredCopy parseEntireProject
