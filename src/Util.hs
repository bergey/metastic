{-# LANGUAGE OverloadedStrings #-}

module Util where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Time
import System.Directory
import System.FilePath
import Data.RDF.Namespace
import System.Locale
import Network.URI hiding (query)

import Data.RDF

subject,verb,object :: Triple -> Node
subject (Triple s _ _) = s
verb    (Triple _ v _) = v
object  (Triple _ _ o) = o

lText :: LValue -> Text
lText (PlainL t) = t
lText (PlainLL t _) = t
lText (TypedL t _) = t

globalPrefix :: PrefixMappings
globalPrefix = ns_mappings []

baseURL :: Maybe BaseUrl
baseURL = Just $ BaseUrl "file://"

lookupVerb :: Text -> Node
lookupVerb t = unode t

parse8601 :: ParseTime t => String -> Maybe t
parse8601 = parseTime defaultTimeLocale "%0Y-%m-%dT%H:%M:%S"

dirByDate :: Day -> FilePath
dirByDate = formatTime defaultTimeLocale directoryFormat

directoryFormat :: FilePath
directoryFormat = "%Y" </> "%Y-%m-%d"

matchExtension :: String -> FilePath -> Bool
matchExtension e f = (T.toCaseFold . T.pack $ e)
                     == (T.toCaseFold . T.pack . takeExtension $ f)

escapePath :: String -> String
escapePath = escapeURIString (flip Prelude.notElem  " &#?@()")

fileURI :: String -> IO Text
fileURI f = do
  path <- canonicalizePath f
  return $ T.append "file://" (T.pack . escapePath $ path)

turtleFname :: FilePath -> FilePath
turtleFname f = replaceExtension f "ttl"
