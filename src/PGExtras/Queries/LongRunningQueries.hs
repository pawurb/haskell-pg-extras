{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module PGExtras.Queries.LongRunningQueries (longRunningQueriesSQL, displayLongRunningQueries) where

import PGExtras.Helpers (maybeInt, maybeText, maybeZonedTime)
import Database.PostgreSQL.Simple
import Text.RawString.QQ
import qualified Data.Text as Text
import Control.Monad (forM_)
import Data.List (intercalate)
import Data.Time (ZonedTime)

longRunningQueriesSQL :: Query
longRunningQueriesSQL = [r|SELECT
  pid,
  pg_stat_activity.query_start,
  query AS query
FROM
  pg_stat_activity
WHERE
  pg_stat_activity.query <> ''::text
  AND state <> 'idle'
  AND now() - pg_stat_activity.query_start > interval '5 minutes'
ORDER BY
  now() - pg_stat_activity.query_start DESC;|]

displayLongRunningQueries :: [(Maybe Int, Maybe ZonedTime, Maybe Text.Text)] -> IO ()
displayLongRunningQueries rows = do
  putStrLn $ description
  putStrLn $ intercalate " | " tableHeaders
  forM_ rows $ \(arg1, arg2, arg3) ->
    putStrLn $ maybeInt(arg1) ++ " | " ++ maybeZonedTime(arg2) ++ " | " ++ maybeText(arg3)

description :: [Char]
description = "All queries longer than five minutes by descending duration"

tableHeaders :: [[Char]]
tableHeaders = ["pid", "query_start", "query"]

