{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module PGExtras.Queries.CacheHit (cacheHitSQL, displayCacheHit) where

import PGExtras.Helpers (maybeText)
import Database.PostgreSQL.Simple
import Text.RawString.QQ
import qualified Data.Text as Text
import Control.Monad (forM_)
import Data.List (intercalate)

cacheHitSQL :: Query
cacheHitSQL = [r|SELECT
'index hit rate' AS name,
(sum(idx_blks_hit)) / nullif(sum(idx_blks_hit + idx_blks_read),0) AS ratio
FROM pg_statio_user_indexes
UNION ALL
SELECT 'table hit rate' AS name,
sum(heap_blks_hit) / nullif(sum(heap_blks_hit) + sum(heap_blks_read),0) AS ratio
FROM pg_statio_user_tables;|]

displayCacheHit :: [(Maybe Text.Text, Maybe Text.Text)] -> IO ()
displayCacheHit rows = do
  putStrLn $ description
  putStrLn $ intercalate " | " tableHeaders
  forM_ rows $ \(arg1, arg2) ->
    putStrLn $ maybeText(arg1) ++ " | " ++ maybeText(arg2)

description :: [Char]
description = "Index and table hit rate"

tableHeaders :: [[Char]]
tableHeaders = ["name", "ratio"]
