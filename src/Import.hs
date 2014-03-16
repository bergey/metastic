{-# LANGUAGE OverloadedStrings #-}

module Import where

import Control.Applicative
import Control.Lens

import Data.Aeson hiding (object)
import qualified Data.HashMap.Strict as H
import Data.Scientific
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)

import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text as T
import Data.Text (Text)

import System.Directory
import System.FilePath
import System.IO
import System.Process

import Data.RDF
import Data.RDF.TriplesGraph
import Data.RDF.Namespace

import Util

toText :: Value -> Text
toText (String t) = t
toText (Number n) = toStrict . toLazyText . scientificBuilder $ n
toText val = T.pack . show $ val

withFixedSubject :: Text -> Maybe Value -> [Triple]
withFixedSubject s (Just (Object map)) = [triple (unode  s)
                                          (lookupVerb $ fst po)
                                          (LNode . PlainL . toText . snd $ po) | po <- H.toList map]
withFixedSubject _ _ = []

tika :: FilePath -> IO (Maybe Value)
tika f = do
  let p = (proc "tika" ["-j", f]) {std_out=CreatePipe}
  (_, Just out, _, _) <- createProcess p
  decode <$> BS.hGetContents out

readFileMeta :: FilePath -> IO TriplesGraph
readFileMeta f = do
  j <- tika f
  s <- fileURI f
  let tris = withFixedSubject s j
      graph = mkRdf tris baseURL globalPrefix :: TriplesGraph
  return graph
