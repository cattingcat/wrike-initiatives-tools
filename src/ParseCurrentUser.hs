{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ParseCurrentUser where

import Prelude hiding (readFile, FilePath)
import Control.Monad (filterM)
import Filesystem
import Filesystem.Path
import Filesystem.Path.CurrentOS
import qualified Text.Parsec as P
import Control.Applicative ((<|>))
import Control.Monad.Identity (Identity(..))
import Text.Parsec.Error (ParseError)
import Data.Either (isRight)


parseCurrentUserVar :: P.Parsec String () String
parseCurrentUserVar =
  P.spaces *>
  P.optional (P.string "final") *>
  P.spaces *>
  P.optional (P.string "dal.") *>
  P.string "CurrentUser" *>
  P.spaces *>
  P.many (P.alphaNum <|> P.char '_') <*
  P.spaces <*
  P.char ';'

hasCurrentUserVar :: String -> Bool
hasCurrentUserVar s = any (isRight . P.parse parseCurrentUserVar "hasCurrentUserVar") (lines s)




tst1 :: Either ParseError String
tst1 = P.parse parseCurrentUserVar "" "\n  CurrentUser _3453rert  ; "

tst2 :: Either ParseError String
tst2 =  P.parse parseCurrentUserVar "" "\n  dal.CurrentUser _3453rert  ; "

tst3 :: Bool
tst3 =  hasCurrentUserVar "inport 'puk'; 'nclass Kek { \n  dal.CurrentUser _3453rert  ; \n } "

tst4 :: Bool
tst4 =  hasCurrentUserVar "inport 'puk'; 'nclass Kek { \n  final  dal.CurrentUser _3453rert  ; \n } "