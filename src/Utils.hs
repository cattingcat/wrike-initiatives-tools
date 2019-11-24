{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils (
  printList,
  pubspec,
  anyM,
  flatFileTree,
  dropNothings
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


printList :: Show a => [a] -> IO ()
printList [] = pure ()
printList (a:as) = print a >> printList as

pubspec :: FilePath
pubspec = "pubspec.yaml";

dropNothings :: IO [Maybe a] -> IO [a]
dropNothings ioa = (foldr \case { Just a -> (a:); _ -> id } []) <$> ioa

anyM :: (Monad m, Traversable t) => (a -> m Bool) -> t a -> m Bool
anyM f ta = do
  tb <- traverse f ta
  pure $ any id tb


flatFileTree :: FilePath -> IO [FilePath]
flatFileTree p = do
  folderContent <- listDirectory p
  loop folderContent where
    loop [] = pure []
    loop (a:as) = do
      isDir <- isDirectory a
      isFile <- isFile a
      let isHidden  = either (const True) (T.isPrefixOf ".") $ toText (last . splitDirectories $ a)
      if isDir && not isHidden then do
        subdirs <- listDirectory a
        (<>) <$> loop subdirs <*> loop as
      else if isFile && not isHidden then
        (:) <$> pure a <*> loop as
      else
        loop as