module Export where

import System.IO

import Data.RDF

import Util

writeTurtle :: RDF r => r -> IO ()
writeTurtle = hWriteTurtle stdin

fWriteTurtle :: RDF r => FilePath -> r -> IO ()
fWriteTurtle t rdf = withFile t AppendMode $ \h -> hWriteTurtle h rdf

hWriteTurtle :: RDF r => Handle -> r -> IO ()
hWriteTurtle = hWriteRdf (TurtleSerializer Nothing globalPrefix)

indexFile :: FilePath
indexFile = "/home/bergey/.metastic/index.ttl"

appendIndex :: TriplesGraph -> IO ()
appendIndex = fWriteTurtle indexFile
