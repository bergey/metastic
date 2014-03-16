{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Control.Monad (filterM, (<=<))
import Data.Monoid
import           System.Directory
import           System.Environment

import           Export
import Import

-- example paths, remove after testing
directoryRoot :: FilePath
directoryRoot = "/home/bergey/Library/photos"

main :: IO ()
main = do
  args <- getArgs
  case args of
      ("show":rest)  -> showArgs rest
      ("index":rest) -> index rest
      _              -> T.putStrLn helpText

helpText :: T.Text
helpText = "usage: meta MODE MODEARGS\n\
\\n\
\  where MODE is:\n\
\    show: print metadata on FILENAMES\n\
\    mime: print an email in (TODO) emacs lisp SExp format\n\
\\n\
\  show FILENAME [FILENAMES]:\n\
\    print metadata on one or more files, in Turtle format, to STDOUT"

data ShowConf = ShowConf

showArgs :: [String] -> IO ()
showArgs = showMode ShowConf

showMode :: ShowConf -> [String] -> IO ()
showMode _ = mapM_ (writeTurtle <=< readFileMeta)

index :: [String] -> IO ()
index fns = do
    print fns
    fns' <- filterM doesFileExist fns
    print fns'
    rdfs <-  mapM readFileMeta fns'
    appendIndex $ mconcat rdfs
