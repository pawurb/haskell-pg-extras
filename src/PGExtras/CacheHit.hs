{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module PGExtras.CacheHit (cacheHitSQL, displayCacheHit) where

import PGExtras.Helpers (displayColumns2)
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
  forM_ rows $ displayColumns2

description :: [Char]
description = "Available and installed extensions"

tableHeaders :: [[Char]]
tableHeaders = ["name", "ratio"]
