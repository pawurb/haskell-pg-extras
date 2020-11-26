{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module PGExtras.Queries.AllLocks (allLocksSQL, displayAllLocks) where

import PGExtras.Helpers (maybeText, maybeInt, maybeBool, maybeZonedTime)
import Database.PostgreSQL.Simple
import Text.RawString.QQ
import qualified Data.Text as Text
import Data.Time (ZonedTime)
import Control.Monad (forM_)
import Data.List (intercalate)

allLocksSQL :: Query
allLocksSQL = [r|SELECT
  pg_stat_activity.pid,
  pg_class.relname,
  pg_locks.transactionid,
  pg_locks.granted,
  pg_locks.mode,
  pg_stat_activity.query AS query_snippet,
  pg_stat_activity.query_start
FROM pg_stat_activity,pg_locks left
OUTER JOIN pg_class
  ON (pg_locks.relation = pg_class.oid)
WHERE pg_stat_activity.query <> '<insufficient privilege>'
  AND pg_locks.pid = pg_stat_activity.pid
  AND pg_stat_activity.pid <> pg_backend_pid() order by query_start;|]

displayAllLocks :: [(Maybe Int, Maybe Text.Text, Maybe Text.Text, Maybe Bool, Maybe Text.Text, Maybe Text.Text, Maybe ZonedTime)] -> IO ()
displayAllLocks rows = do
  putStrLn $ description
  putStrLn $ intercalate " | " tableHeaders
  forM_ rows $ \(arg1, arg2, arg3, arg4, arg5, arg6, arg7) ->
    putStrLn $ maybeInt(arg1) ++ " | " ++ maybeText(arg2) ++ " | " ++ maybeText(arg3) ++ " | " ++ maybeBool(arg4) ++ " | " ++ maybeText(arg5) ++ " | " ++ maybeText(arg6) ++ " | " ++ maybeZonedTime(arg7)

description :: [Char]
description = "Queries with active locks"

tableHeaders :: [[Char]]
tableHeaders = ["procpid", "relname", "transactionid", "granted", "query_snippet", "mode", "query_start"]

