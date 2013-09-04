module Export where

import System.IO

import Data.RDF
import Text.RDF.RDF4H.TurtleSerializer
import Text.RDF.RDF4H.NTriplesSerializer

import Util
import Import

writeTurtle :: FilePath -> IO ()
writeTurtle f = do
  rdf <- readFileMeta f
  withFile (turtleFname f) WriteMode $ \h ->
    writeRdf (TurtleSerializer Nothing globalPrefix) rdf

fWriteTurtle :: FilePath -> FilePath -> IO ()
fWriteTurtle t f = withFile t AppendMode $ \h -> hWriteTurtle h f

hWriteTurtle :: Handle -> FilePath -> IO ()
hWriteTurtle h f = do
  rdf <- readFileMeta f
  hWriteRdf (TurtleSerializer Nothing globalPrefix) h rdf
