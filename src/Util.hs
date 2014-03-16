{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Util where

import qualified Data.Map as M
import           Data.Monoid
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Time
import           System.Directory
import           System.FilePath
import           Data.RDF.Namespace
import           System.Locale
import           Network.URI hiding (query)

import           Data.RDF

lText :: LValue -> Text
lText (PlainL t) = t
lText (PlainLL t _) = t
lText (TypedL t _) = t

globalPrefix :: PrefixMappings
globalPrefix = ns_mappings []

baseURL :: Maybe BaseUrl
baseURL = Just $ BaseUrl "file://"

lookupVerb :: Text -> Node
lookupVerb = unode

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
escapePath = escapeURIString (`Prelude.notElem`  " &#?@()")

fileURI :: String -> IO Text
fileURI f = do
  p <- canonicalizePath f
  return $ T.append "file://" (T.pack . escapePath $ p)

turtleFname :: FilePath -> FilePath
turtleFname f = replaceExtension f "ttl"

instance RDF r => Monoid r where
    mempty = empty
    mappend a b =
        mkRdf (triplesOf a ++ triplesOf b) ns prefices where
          ns = case (baseUrl a, baseUrl b) of
              (Just u, Nothing) -> Just u
              (Just u1, Just u2) -> if u1==u2
                                    then Just u1
                                    else Nothing
              (Nothing, u) -> u
          prefices = case (prefixMappings a, prefixMappings b) of
              (PrefixMappings a', PrefixMappings b') -> PrefixMappings (M.union a' b')
