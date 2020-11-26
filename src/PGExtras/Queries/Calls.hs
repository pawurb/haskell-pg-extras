{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module PGExtras.Queries.Calls (callsSQL, displayCalls) where

import PGExtras.Helpers (maybeText)
import Database.PostgreSQL.Simple
import Text.RawString.QQ
import qualified Data.Text as Text
import Control.Monad (forM_)
import Data.List (intercalate)

callsSQL :: Query
callsSQL = [r|SELECT query AS qry,
interval '1 millisecond' * total_time AS exec_time,
to_char((total_time/sum(total_time) OVER()) * 100, 'FM90D0') || '%'  AS prop_exec_time,
to_char(calls, 'FM999G999G990') AS ncalls,
interval '1 millisecond' * (blk_read_time + blk_write_time) AS sync_io_time
FROM pg_stat_statements WHERE userid = (SELECT usesysid FROM pg_user WHERE usename = current_user LIMIT 1)
ORDER BY calls DESC LIMIT 10;|]

displayCalls :: [(Maybe Text.Text, Maybe Text.Text, Maybe Text.Text, Maybe Text.Text, Maybe Text.Text)] -> IO ()
displayCalls rows = do
  putStrLn $ description
  putStrLn $ intercalate " | " tableHeaders
  forM_ rows $ \(arg1, arg2, arg3, arg4, arg5) ->
    putStrLn $ maybeText(arg1) ++ " | " ++ maybeText(arg2) ++ " | " ++ maybeText(arg3) ++ " | " ++ maybeText(arg4) ++ " | " ++ maybeText(arg5)

description :: [Char]
description = "10 queries that have highest frequency of execution"

tableHeaders :: [[Char]]
tableHeaders = ["qry", "exec_time", "prop_exec_time", "ncalls", "sync_io_time"]
