module CmdLine where

import System.Directory
import System.FilePath

import Util

main :: IO ()
main = do
  let fromDir =  "/home/bergey/Library/photos"
  files <- getDirectoryContents fromDir
  mapM_ (fileByDate . (fromDir </>)) $ filter (matchExtension ".jpg") files

-- example paths, remove after testing
directoryRoot = "/home/bergey/Library/photos"

mp3 = "/home/bergey/Music/Simon & Garfunkel/Sounds Of Silence/01.he Sounds Of Silence.mp3"
jpg = "/home/bergey/Library/photos/IMG_1734.JPG"
