{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module PGExtras.Queries.IndexUsage (indexUsageSQL, displayIndexUsage) where

import PGExtras.Helpers (maybeText, maybeInt)
import Database.PostgreSQL.Simple
import Text.RawString.QQ
import qualified Data.Text as Text
import Control.Monad (forM_)
import Data.List (intercalate)

indexUsageSQL :: Query
indexUsageSQL = [r|SELECT relname,
   CASE idx_scan
     WHEN 0 THEN 'Insufficient data'
     ELSE (100 * idx_scan / (seq_scan + idx_scan))::text
   END percent_of_times_index_used,
   n_live_tup rows_in_table
 FROM
   pg_stat_user_tables
 ORDER BY
   n_live_tup DESC;|]

displayIndexUsage :: [(Maybe Text.Text, Maybe Text.Text, Maybe Int)] -> IO ()
displayIndexUsage rows = do
  putStrLn $ description
  putStrLn $ intercalate " | " tableHeaders
  forM_ rows $ \(arg1, arg2, arg3) ->
    putStrLn $ maybeText(arg1) ++ " | " ++ maybeText(arg2)++ " | " ++ maybeInt(arg3)

description :: [Char]
description = "Index hit rate (effective databases are at 99% and up)"

tableHeaders :: [[Char]]
tableHeaders = ["relname", "percent_of_times_index_used", "rows_in_table"]

