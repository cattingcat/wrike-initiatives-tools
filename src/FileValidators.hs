{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module FileValidators where

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
import ParseCurrentUser


validateProjectFiles :: ProjectPath -> IO Bool
validateProjectFiles p = isJust <$> visitDartFiles p validateFile where
  validateFile p = do
    content <- readFile p
    let
      containsDal = B.isInfixOf "wrike_dal" content
      containsDalCore = B.isInfixOf "wrike_dal_core" content
      containsCurrUser = B.isInfixOf "CurrentUser" content
      hasVar = hasCurrentUserVar $ T.unpack (decodeUtf8 content)
--    if containsCurrUser && not hasVar then print ("Has entry, but ho var " ++ show p) else pure ()
    if (containsDal || containsDalCore) && containsCurrUser && hasVar 
    then pure $ Just True
    else pure Nothing
