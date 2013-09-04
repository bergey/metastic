{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory
import System.Environment
import System.FilePath

import Util
import Export

-- example paths, remove after testing
directoryRoot = "/home/bergey/Library/photos"

mp3 = "/home/bergey/Music/Simon & Garfunkel/Sounds Of Silence/01.he Sounds Of Silence.mp3"
jpg = "/home/bergey/Library/photos/IMG_1734.JPG"

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> T.putStrLn helpText
    _ ->
      case head args of
        "show" -> showArgs $ tail args
        otherwise -> T.putStrLn helpText

helpText = "usage: meta MODE MODEARGS\n\
\\n\
\  where MODE is:\n\
\    show: print metadata on FILENAMES\n\
\\n\
\  show FILENAME [FILENAMES]:\n\
\    print metadata on one or more files, in Turtle format, to STDOUT"

data ShowConf = ShowConf

showArgs :: [String] -> IO ()
showArgs = showMode ShowConf

showMode :: showConf -> [String] -> IO ()
showMode _ args = mapM_ writeTurtle args
