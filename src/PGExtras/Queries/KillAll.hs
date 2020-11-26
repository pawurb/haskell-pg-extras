{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module PGExtras.Queries.KillAll (killAllSQL, displayKillAll) where

import PGExtras.Helpers (maybeBool)
import Database.PostgreSQL.Simple
import Text.RawString.QQ
import qualified Data.Text as Text
import Control.Monad (forM_)
import Data.List (intercalate)

killAllSQL :: Query
killAllSQL = [r|SELECT pg_terminate_backend(pid), 't' as t FROM pg_stat_activity
  WHERE pid <> pg_backend_pid()
  AND query <> '<insufficient privilege>'
  AND datname = current_database();|]

displayKillAll :: [(Maybe Bool, Maybe Text.Text)] -> IO ()
displayKillAll rows = do
  putStrLn $ description
  putStrLn $ intercalate " | " tableHeaders
  forM_ rows $ \(arg1, _) ->
    putStrLn $ maybeBool(arg1)

description :: [Char]
description = "Kill all the active database connections"

tableHeaders :: [[Char]]
tableHeaders = ["success"]

