{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module PGExtras.Queries.Blocking (blockingSQL, displayBlocking) where

import PGExtras.Helpers (maybeText)
import Database.PostgreSQL.Simple
import Text.RawString.QQ
import qualified Data.Text as Text
import Control.Monad (forM_)
import Data.List (intercalate)

blockingSQL :: Query
blockingSQL = [r|SELECT bl.pid AS blocked_pid,
  ka.query AS blocking_statement,
  now() - ka.query_start AS blocking_duration,
  kl.pid AS blocking_pid,
  a.query AS blocked_statement,
  now() - a.query_start AS blocked_duration
FROM pg_catalog.pg_locks bl
JOIN pg_catalog.pg_stat_activity a
  ON bl.pid = a.pid
JOIN pg_catalog.pg_locks kl
  JOIN pg_catalog.pg_stat_activity ka
    ON kl.pid = ka.pid
ON bl.transactionid = kl.transactionid AND bl.pid != kl.pid
WHERE NOT bl.granted;|]

displayBlocking :: [(Maybe Text.Text, Maybe Text.Text, Maybe Text.Text, Maybe Text.Text, Maybe Text.Text, Maybe Text.Text)] -> IO ()
displayBlocking rows = do
  putStrLn $ description
  putStrLn $ intercalate " | " tableHeaders
  forM_ rows $ \(arg1, arg2, arg3, arg4, arg5, arg6) ->
    putStrLn $ maybeText(arg1) ++ " | " ++ maybeText(arg2) ++ " | " ++ maybeText(arg3) ++ " | " ++ maybeText(arg4) ++ " | " ++ maybeText(arg5) ++ " | " ++ maybeText(arg6)

description :: [Char]
description = "Queries holding locks other queries are waiting to be released"

tableHeaders :: [[Char]]
tableHeaders = ["blocked_pid", "blocking_statement", "blocking_duration", "blocking_pid", "blocked_statement", "blocked_duration"]
