{-# LANGUAGE OverloadedStrings #-}

module PGExtras.Extensions (extensionsSQL, displayExtensions) where

import Database.PostgreSQL.Simple
import PGExtras.Helpers (displayColumns4)
import Control.Monad (forM_)
import qualified Data.Text as Text
import Data.List (intercalate)

extensionsSQL :: Query
extensionsSQL = "SELECT name, default_version, installed_version, comment FROM pg_available_extensions ORDER BY installed_version;"

displayExtensions :: [(Maybe Text.Text, Maybe Text.Text, Maybe Text.Text, Maybe Text.Text)] -> IO ()
displayExtensions rows = do
  putStrLn $ description
  putStrLn $ intercalate " | " tableHeaders
  forM_ rows $ displayColumns4

description :: [Char]
description = "Available and installed extensions"

tableHeaders :: [[Char]]
tableHeaders = ["name", "default_version", "installed_version", "comment"]
