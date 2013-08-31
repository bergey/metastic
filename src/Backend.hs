{-# LANGUAGE OverloadedStrings #-}

module Backend where

import System.Process

import Util
import Export

importFileMeta :: FilePath -> IO ProcessHandle
importFileMeta f = do
  writeTurtle f
  runCommand $ "4s-import test '" ++ turtleFname f ++ "'"
