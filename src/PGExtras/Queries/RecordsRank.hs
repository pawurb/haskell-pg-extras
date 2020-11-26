{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module PGExtras.Queries.RecordsRank (recordsRankSQL, displayRecordsRank) where

import PGExtras.Helpers (maybeText, maybeInt)
import Database.PostgreSQL.Simple
import Text.RawString.QQ
import qualified Data.Text as Text
import Control.Monad (forM_)
import Data.List (intercalate)

recordsRankSQL :: Query
recordsRankSQL = [r|SELECT
  relname AS name,
  n_live_tup AS estimated_count
FROM
  pg_stat_user_tables
ORDER BY
  n_live_tup DESC;|]

displayRecordsRank :: [(Maybe Text.Text, Maybe Int)] -> IO ()
displayRecordsRank rows = do
  putStrLn $ description
  putStrLn $ intercalate " | " tableHeaders
  forM_ rows $ \(arg1, arg2) ->
    putStrLn $ maybeText(arg1) ++ " | " ++ maybeInt(arg2)

description :: [Char]
description = "All tables and the number of rows in each ordered by number of rows descending"

tableHeaders :: [[Char]]
tableHeaders = ["name", "estimated_count"]
