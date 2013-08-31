{-# LANGUAGE OverloadedStrings #-}

module Refile where

import Control.Applicative
import Data.Text as T

import System.Directory
import System.FilePath

import Data.RDF

import Util
import Import


fileByDate :: FilePath -> FilePath -> IO ()
fileByDate directoryRoot f = do
  putStrLn f
  tris <- readFileMeta f
  case query tris Nothing (Just . unode $ "meta:creation-date") Nothing of
    [] -> return ()
    (tri:_) -> case object tri of
      LNode l ->   case dirByDate <$> (parse8601 . T.unpack . lText $ l) of
        Nothing -> return ()
        Just dir -> do
          let dirPath = directoryRoot </> dir
          createDirectoryIfMissing True dirPath
          renameFile f (dirPath </> takeFileName f)
