{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Systemd.Journal.Upload where

import Data.Aeson (FromJSON)
import Data.ByteString (ByteString)
import Data.ByteString.Base64 (decodeBase64, encodeBase64)
import Data.Foldable (foldr')
import qualified Data.HashMap.Strict as Map (lookup)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Pipes (Pipe)
import qualified Pipes.Prelude as PP
import Systemd.Journal

filterFieldPipe :: Functor m => JournalField -> (ByteString -> Bool) -> Pipe JournalEntry JournalEntry m r
filterFieldPipe field predicate =
  PP.filter (maybe False predicate . Map.lookup field . journalEntryFields)

anyMatch :: NonEmpty Filter -> Filter
anyMatch (f1 :| fs) = foldr' Or f1 fs

priorityFilter :: Priority -> Filter
priorityFilter = Match (mkJournalField "PRIORITY") . encodePriority

upToPriority :: Priority -> Filter
upToPriority pr =
  let preds = init $ enumFromTo minBound pr
   in anyMatch $ fmap priorityFilter $ pr :| preds
