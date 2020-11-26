{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module PGExtras.Queries.UnusedIndexes (unusedIndexesSQL, displayUnusedIndexes) where

import PGExtras.Helpers (maybeText, maybeInt)
import Database.PostgreSQL.Simple
import Text.RawString.QQ
import qualified Data.Text as Text
import Control.Monad (forM_)
import Data.List (intercalate)

unusedIndexesSQL :: Query
unusedIndexesSQL = [r|SELECT
  schemaname || '.' || relname AS table,
  indexrelname AS index,
  pg_size_pretty(pg_relation_size(i.indexrelid)) AS index_size,
  idx_scan as index_scans
FROM pg_stat_user_indexes ui
JOIN pg_index i ON ui.indexrelid = i.indexrelid
WHERE NOT indisunique AND idx_scan < 50 AND pg_relation_size(relid) > 5 * 8192
ORDER BY pg_relation_size(i.indexrelid) / nullif(idx_scan, 0) DESC NULLS FIRST,
pg_relation_size(i.indexrelid) DESC;|]

displayUnusedIndexes :: [(Maybe Text.Text, Maybe Text.Text, Maybe Text.Text, Maybe Int)] -> IO ()
displayUnusedIndexes rows = do
  putStrLn $ description
  putStrLn $ intercalate " | " tableHeaders
  forM_ rows $ \(arg1, arg2, arg3, arg4) ->
    putStrLn $ maybeText(arg1) ++ " | " ++ maybeText(arg2) ++ " | " ++ maybeText(arg3) ++ " | " ++ maybeInt(arg4)

description :: [Char]
description = "Unused and almost unused indexes"

tableHeaders :: [[Char]]
tableHeaders = ["name", "ratio"]
