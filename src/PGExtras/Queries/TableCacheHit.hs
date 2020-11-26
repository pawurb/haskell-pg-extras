{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module PGExtras.Queries.TableCacheHit (tableCacheHitSQL, displayTableCacheHit) where

import PGExtras.Helpers (maybeText, maybeInt)
import Database.PostgreSQL.Simple
import Text.RawString.QQ
import qualified Data.Text as Text
import Control.Monad (forM_)
import Data.List (intercalate)

tableCacheHitSQL :: Query
tableCacheHitSQL = [r|SELECT
  relname AS name,
  heap_blks_hit AS buffer_hits,
  heap_blks_read AS block_reads,
  heap_blks_hit + heap_blks_read AS total_read,
  CASE (heap_blks_hit + heap_blks_read)::float
    WHEN 0 THEN 'Insufficient data'
    ELSE (heap_blks_hit / (heap_blks_hit + heap_blks_read)::float)::text
  END ratio
FROM
  pg_statio_user_tables
ORDER BY
  heap_blks_hit / (heap_blks_hit + heap_blks_read + 1)::float DESC;|]

displayTableCacheHit :: [(Maybe Text.Text, Maybe Int, Maybe Int, Maybe Int, Maybe Text.Text)] -> IO ()
displayTableCacheHit rows = do
  putStrLn $ description
  putStrLn $ intercalate " | " tableHeaders
  forM_ rows $ \(arg1, arg2, arg3, arg4, arg5) ->
    putStrLn $ maybeText(arg1) ++ " | " ++ maybeInt(arg2) ++ " | " ++ maybeInt(arg3) ++ " | " ++ maybeInt(arg4) ++ " | " ++ maybeText(arg5)

description :: [Char]
description = "Calculates your cache hit rate for reading tables"

tableHeaders :: [[Char]]
tableHeaders = ["name", "buffer_hits", "block_reads", "total_read", "ratio"]
