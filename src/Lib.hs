module Lib (
  analyzeProjects
) where

import Prelude hiding (readFile, FilePath)
import Control.Monad (filterM)
import Filesystem
import Filesystem.Path
import Filesystem.Path.CurrentOS
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.ByteString as B
import Data.Maybe (isJust)
import qualified Text.Parsec as Parsec
import Control.Applicative ((<|>))

import Utils
import Core
import ProjectValidators
import FileValidators


analyzeProjects :: FilePath -> IO ()
analyzeProjects projectsRoot = do
  dirs <- listDirectory projectsRoot
  dartProjects <- filterDartProjects dirs
  withDal <- filterM lookForDalDependency dartProjects
  hasCurrUser <- mapM validateProjectFiles withDal
  let
    z = zip withDal hasCurrUser
    validProjectsPair = filter snd z
    validProjects = fmap (projectDir . fst) validProjectsPair

  printList validProjects
  print $ length validProjects
  pure ()


filterDartProjects :: [FilePath] -> IO [ProjectPath]
filterDartProjects ps = dropNothings $ mapM validate ps where
  validate a = do
    mpp <- checkIsDartProject a
    case mpp of
      Just projPath -> pure (Just projPath)
      _             -> pure Nothing
