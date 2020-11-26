{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module PGExtras.Queries.IndexSize (indexSizeSQL, displayIndexSize) where

import PGExtras.Helpers (maybeText)
import Database.PostgreSQL.Simple
import Text.RawString.QQ
import qualified Data.Text as Text
import Control.Monad (forM_)
import Data.List (intercalate)

indexSizeSQL :: Query
indexSizeSQL = [r|SELECT c.relname AS name,
  pg_size_pretty(sum(c.relpages::bigint*8192)::bigint) AS size
FROM pg_class c
LEFT JOIN pg_namespace n ON (n.oid = c.relnamespace)
WHERE n.nspname NOT IN ('pg_catalog', 'information_schema')
AND n.nspname !~ '^pg_toast'
AND c.relkind='i'
GROUP BY c.relname
ORDER BY sum(c.relpages) DESC;|]

displayIndexSize :: [(Maybe Text.Text, Maybe Text.Text)] -> IO ()
displayIndexSize rows = do
  putStrLn $ description
  putStrLn $ intercalate " | " tableHeaders
  forM_ rows $ \(arg1, arg2) ->
    putStrLn $ maybeText(arg1) ++ " | " ++ maybeText(arg2)

description :: [Char]
description = "The size of indexes, descending by size"

tableHeaders :: [[Char]]
tableHeaders = ["name", "ratio"]
