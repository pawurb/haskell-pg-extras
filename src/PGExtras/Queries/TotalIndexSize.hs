{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module PGExtras.Queries.TotalIndexSize (totalIndexSizeSQL, displayTotalIndexSize) where

import PGExtras.Helpers (maybeText)
import Database.PostgreSQL.Simple
import Text.RawString.QQ
import qualified Data.Text as Text
import Control.Monad (forM_)
import Data.List (intercalate)

totalIndexSizeSQL :: Query
totalIndexSizeSQL = [r|SELECT pg_size_pretty(sum(c.relpages::bigint*8192)::bigint) AS size,
  't' as t
FROM pg_class c
LEFT JOIN pg_namespace n ON (n.oid = c.relnamespace)
WHERE n.nspname NOT IN ('pg_catalog', 'information_schema')
AND n.nspname !~ '^pg_toast'
AND c.relkind='i';|]

displayTotalIndexSize :: [(Maybe Text.Text, Maybe Text.Text)] -> IO ()
displayTotalIndexSize rows = do
  putStrLn $ description
  putStrLn $ intercalate " | " tableHeaders
  forM_ rows $ \(arg1, _) ->
    putStrLn $ maybeText(arg1)

description :: [Char]
description = "Total size of all indexes in MB"

tableHeaders :: [[Char]]
tableHeaders = ["size"]
