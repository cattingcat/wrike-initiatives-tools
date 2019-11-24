module PubspecValidators where

import Prelude hiding (readFile, FilePath)
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


checkPubspecDeps :: T.Text -> FilePath -> IO Bool
checkPubspecDeps depName pubspecPath = do
  content <- readFile pubspecPath
  pure $ B.isInfixOf (encodeUtf8 depName) content -- TODO: Replace with yaml parser
