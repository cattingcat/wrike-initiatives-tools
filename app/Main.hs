{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (readFile, FilePath)

import Filesystem
import Filesystem.Path
import Filesystem.Path.CurrentOS

import Lib



projectsFolderPath :: FilePath
projectsFolderPath = fromText "/Users/mark.martynov/Desktop/fe-repos/frontend"

main :: IO ()
main = analyzeProjects projectsFolderPath
