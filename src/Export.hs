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
    -- hWriteRdf (TurtleSerializer Nothing globalPrefix) h rdf
    writeRdf NTriplesSerializer rdf
