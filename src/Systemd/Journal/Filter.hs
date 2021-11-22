{-# LANGUAGE OverloadedStrings #-}

module Systemd.Journal.Filter where

import Data.ByteString (ByteString)
import Data.Foldable (foldr')
import qualified Data.HashMap.Strict as Map (lookup)
import Data.List.NonEmpty (NonEmpty ((:|)))
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
