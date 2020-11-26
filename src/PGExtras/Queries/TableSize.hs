{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module PGExtras.Queries.TableSize (tableSizeSQL, displayTableSize) where

import PGExtras.Helpers (maybeText)
import Database.PostgreSQL.Simple
import Text.RawString.QQ
import qualified Data.Text as Text
import Control.Monad (forM_)
import Data.List (intercalate)

tableSizeSQL :: Query
tableSizeSQL = [r|SELECT c.relname AS name,
  pg_size_pretty(pg_table_size(c.oid)) AS size
FROM pg_class c
LEFT JOIN pg_namespace n ON (n.oid = c.relnamespace)
WHERE n.nspname NOT IN ('pg_catalog', 'information_schema')
AND n.nspname !~ '^pg_toast'
AND c.relkind IN ('r', 'm')
ORDER BY pg_table_size(c.oid) DESC;|]

displayTableSize :: [(Maybe Text.Text, Maybe Text.Text)] -> IO ()
displayTableSize rows = do
  putStrLn $ description
  putStrLn $ intercalate " | " tableHeaders
  forM_ rows $ \(arg1, arg2) ->
    putStrLn $ maybeText(arg1) ++ " | " ++ maybeText(arg2)

description :: [Char]
description = "Size of the tables (excluding indexes), descending by size"

tableHeaders :: [[Char]]
tableHeaders = ["name", "size"]
