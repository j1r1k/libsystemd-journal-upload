{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Systemd.Journal.Filter where

import Data.Foldable (foldr')
import qualified Data.HashMap.Strict as Map (lookup)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import Data.Text.Encoding (decodeLatin1)
import qualified Text.Regex.Pcre2 as PCRE (matches)
import Systemd.Journal (Filter(Or, Match), JournalEntry(journalEntryFields), Priority, mkJournalField, encodePriority)

anyMatch :: NonEmpty Filter -> Filter
anyMatch (f1 :| fs) = foldr' Or f1 fs

priorityFilter :: Priority -> Filter
priorityFilter = Match (mkJournalField "PRIORITY") . encodePriority

upToPriority :: Priority -> Filter
upToPriority pr =
  let preds = init $ enumFromTo minBound pr
   in anyMatch $ fmap priorityFilter $ pr :| preds

data FieldFilter = RegexFieldFilter {
  rfField :: Text,
  rfRegex :: Text
} deriving (Eq, Show)

matchesFieldFilter :: FieldFilter -> JournalEntry -> Bool
matchesFieldFilter (RegexFieldFilter { rfField, rfRegex}) entry =
      all (PCRE.matches rfRegex . decodeLatin1) $ Map.lookup (mkJournalField rfField) $ journalEntryFields entry

data EntryFilter = 
    NotEntryFilter EntryFilter
  | AndEntryFilter [FieldFilter] 
  deriving (Eq, Show)

matchesEntryFilter :: EntryFilter -> JournalEntry -> Bool
matchesEntryFilter (NotEntryFilter ef) entry = not $ matchesEntryFilter ef entry
matchesEntryFilter (AndEntryFilter fieldFilters) entry =
   all (\ff -> matchesFieldFilter ff entry) fieldFilters
