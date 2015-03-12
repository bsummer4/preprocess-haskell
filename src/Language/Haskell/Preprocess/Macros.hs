{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Language.Haskell.Preprocess.Macros(compilerMacros,stdHdrs) where

import Prelude

import Data.FileEmbed
import Data.String.Here

import           Control.Arrow
import qualified Data.ByteString           as BS
import qualified Data.Map                  as M
import qualified Filesystem.Path           as P
import qualified Filesystem.Path.CurrentOS as P

stdHdrs ∷ M.Map P.FilePath BS.ByteString
stdHdrs = M.fromList $ map (first P.decodeString) $(embedDir "include")

compilerMacros ∷ String
compilerMacros = x++"\n" where x = [here|
#define __GLASGOW_HASKELL__ 708
#define x86_64_HOST_ARCH 1
#define linux_HOST_OS 1
#define mingw32_HOST_OS 1
#define FLT_RADIX 2
#define HAVE_POLL 1
#define INTEGER_GMP 1
|]
