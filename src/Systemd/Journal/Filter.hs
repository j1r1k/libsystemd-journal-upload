{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Systemd.Journal.Filter where

import Data.Aeson (FromJSON (parseJSON), (.:))
import qualified Data.Aeson as A
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

instance FromJSON FieldFilter where
  parseJSON = A.withObject "FieldFilter" $ \o -> do
    rfField <- o .: "field"
    rfRegex <- o .: "regex"
    return RegexFieldFilter {rfField, rfRegex}

matchesFieldFilter :: FieldFilter -> JournalEntry -> Bool
matchesFieldFilter (RegexFieldFilter { rfField, rfRegex}) entry =
      all (PCRE.matches rfRegex . decodeLatin1) $ Map.lookup (mkJournalField rfField) $ journalEntryFields entry

data EntryFilter = 
    NotEntryFilter EntryFilter
  | AndEntryFilter [FieldFilter] 
  deriving (Eq, Show)

instance FromJSON EntryFilter where
  parseJSON = A.withObject "EntryFilter" $ \o -> do
    notFilter <- o .: "not"
    andFilters <- o .: "and"
    return $ case (notFilter, andFilters) of
      (Just nf, Nothing) -> NotEntryFilter nf
      (Nothing, Just af) -> AndEntryFilter af
      _ -> error "EntryFilter must have either 'not' or 'and' key"

matchesEntryFilter :: EntryFilter -> JournalEntry -> Bool
matchesEntryFilter (NotEntryFilter ef) entry = not $ matchesEntryFilter ef entry
matchesEntryFilter (AndEntryFilter fieldFilters) entry =
   all (\ff -> matchesFieldFilter ff entry) fieldFilters
