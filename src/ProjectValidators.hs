{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module ProjectValidators(
  ProjectPath,
  projectDir,
  checkIsDartProject,
  lookForDalDependency,
  visitDartFiles
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
import PubspecValidators


newtype ProjectPath = MkProjectPath { path :: FilePath }
  deriving (Show)

projectDir :: ProjectPath -> FilePath
projectDir = last . splitDirectories . path

checkIsDartProject :: FilePath -> IO (Maybe ProjectPath)
checkIsDartProject p = do
  let localPubspec = p </> pubspec
  dirs <- listDirectory p
  if localPubspec `elem` dirs then pure $ Just (MkProjectPath p) else pure Nothing

lookForDalDependency :: ProjectPath -> IO Bool
lookForDalDependency (MkProjectPath p) = do
  hasDal <- checkPubspecDeps "wrike_dal" (p </> pubspec)
  hasDalCore <- checkPubspecDeps "wrike_dal_core" (p </> pubspec)
  pure $ hasDal || hasDalCore

-- | will stop visiting if callback returns Just
visitDartFiles :: ProjectPath -> (FilePath -> IO (Maybe a)) -> IO (Maybe a)
visitDartFiles (MkProjectPath p) f = do
  files <- flatFileTree p
  let dartFiles = filter (`hasExtension` "dart") files
  loop dartFiles where
    loop [] = pure Nothing
    loop (a:as) = do
      r <- f a
      case r of
        Just a -> pure $ Just a
        _      -> loop as