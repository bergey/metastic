{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Lens

import Data.Aeson
import Data.HashMap.Strict
import Data.Attoparsec.Number

import Network.URI
import Data.ByteString.Lazy as BS
import Data.Text as T

import System.Directory
import System.FilePath
import System.IO
import System.Process

import Data.RDF
import Data.RDF.TriplesGraph
import Data.RDF.Namespace
import Text.RDF.RDF4H.TurtleSerializer
import Text.RDF.RDF4H.NTriplesSerializer

globalPrefix :: PrefixMappings
globalPrefix = ns_mappings []

baseURL :: Maybe BaseUrl
baseURL = Just $ BaseUrl "file://"

testFile = "/home/bergey/Music/Simon & Garfunkel/Sounds Of Silence/meta.json"

testJSON :: IO (Maybe Value)
testJSON = decode <$> BS.readFile testFile

toText :: Value -> Text
toText (String t) = t
toText (Number (I i)) = T.pack . show $ i
toText (Number (D d)) = T.pack . show $ d
toText val = T.pack . show $ val

lookupVerb :: Text -> Node
lookupVerb t = unode t

withFixedSubject :: Text -> Maybe Value -> [Triple]
withFixedSubject s (Just (Object map)) = [Triple (unode  s)
                                          (lookupVerb $ fst po)
                                          (LNode . PlainL . toText . snd $ po) | po <- toList map] 
withFixedSubject _ _ = []

turtleFname :: FilePath -> FilePath
turtleFname f = replaceExtension f "ttl"

escapePath :: String -> String
escapePath = escapeURIString (flip Prelude.notElem  " &#?@()")

fileURI :: String -> IO Text
fileURI f = do
  path <- canonicalizePath f
  return $ T.append "file://" (T.pack . escapePath $ path)

readFileMeta :: FilePath -> IO TriplesGraph
readFileMeta f = do
  j <- decode <$> BS.readFile f
  s <- fileURI (turtleFname f)
  let tris = withFixedSubject s j
      graph = mkRdf tris baseURL globalPrefix :: TriplesGraph
  return graph

writeTurtle :: FilePath -> IO ()
writeTurtle f = do
  rdf <- readFileMeta f
  withFile (turtleFname f) WriteMode $ \h -> 
    -- hWriteRdf (TurtleSerializer Nothing globalPrefix) h rdf
    writeRdf NTriplesSerializer rdf

importFileMeta :: FilePath -> IO ProcessHandle
importFileMeta f = do
  writeTurtle f
  runCommand $ "4s-import test '" ++ turtleFname f ++ "'"
