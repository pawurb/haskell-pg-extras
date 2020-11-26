{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module PGExtras.Queries.SeqScans (seqScansSQL, displaySeqScans) where

import PGExtras.Helpers (maybeText, maybeInt)
import Database.PostgreSQL.Simple
import Text.RawString.QQ
import qualified Data.Text as Text
import Control.Monad (forM_)
import Data.List (intercalate)

seqScansSQL :: Query
seqScansSQL = [r|SELECT
  relname AS name,
  seq_scan as count
FROM
  pg_stat_user_tables
ORDER BY seq_scan DESC;|]

displaySeqScans :: [(Maybe Text.Text, Maybe Int)] -> IO ()
displaySeqScans rows = do
  putStrLn $ description
  putStrLn $ intercalate " | " tableHeaders
  forM_ rows $ \(arg1, arg2) ->
    putStrLn $ maybeText(arg1) ++ " | " ++ maybeInt(arg2)

description :: [Char]
description = "Count of sequential scans by table descending by order"

tableHeaders :: [[Char]]
tableHeaders = ["name", "count"]
