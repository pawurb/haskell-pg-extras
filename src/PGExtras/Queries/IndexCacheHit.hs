{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module PGExtras.Queries.IndexCacheHit (indexCacheHitSQL, displayIndexCacheHit) where

import PGExtras.Helpers (maybeText, maybeInt)
import Database.PostgreSQL.Simple
import Text.RawString.QQ
import qualified Data.Text as Text
import Control.Monad (forM_)
import Data.List (intercalate)

indexCacheHitSQL :: Query
indexCacheHitSQL = [r|SELECT
  relname AS name,
  idx_blks_hit AS buffer_hits,
  idx_blks_read AS block_reads,
  idx_blks_hit + idx_blks_read AS total_read,
  CASE (idx_blks_hit + idx_blks_read)::float
    WHEN 0 THEN 'Insufficient data'
    ELSE (idx_blks_hit / (idx_blks_hit + idx_blks_read)::float)::text
  END ratio
FROM
  pg_statio_user_tables
ORDER BY
  idx_blks_hit / (idx_blks_hit + idx_blks_read + 1)::float DESC;|]

displayIndexCacheHit :: [(Maybe Text.Text, Maybe Int, Maybe Int, Maybe Int, Maybe Text.Text)] -> IO ()
displayIndexCacheHit rows = do
  putStrLn $ description
  putStrLn $ intercalate " | " tableHeaders
  forM_ rows $ \(arg1, arg2, arg3, arg4, arg5) ->
    putStrLn $ maybeText(arg1) ++ " | " ++ maybeInt(arg2) ++ " | " ++ maybeInt(arg3) ++ " | " ++ maybeInt(arg4) ++ " | " ++ maybeText(arg5)

description :: [Char]
description = "Calculates your cache hit rate for reading indexes"

tableHeaders :: [[Char]]
tableHeaders = ["name", "buffer_hits", "block_reads", "total_read", "ratio"]

