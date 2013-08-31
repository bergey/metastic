{-# LANGUAGE OverloadedStrings #-}

import Data.Text as T

import Data.RDF
import Data.RDF.Namespace

globalPrefix :: PrefixMappings
globalPrefix = ns_mappings []

baseURL :: Maybe BaseUrl
baseURL = Just $ BaseUrl "file://"

testURL = "file:///this/is/not/a/palindrome"

tris :: [Triple]
tris = [Triple
       (unode testURL)
       (unode testURL)
       (LNode . PlainL . T.pack $ "literal string")]

testRdf :: TriplesGraph
testRdf = mkRdf tris baseURL globalPrefix

main :: IO ()
main = print testRdf

