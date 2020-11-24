{-# LANGUAGE OverloadedStrings #-}

module PGExtras (
  extrasCacheHit,
  extrasCacheHitRows,
  extrasExtensions,
  extrasExtensionsRows
) where

import PGExtras.CacheHit (cacheHitSQL, displayCacheHit)
import PGExtras.Extensions (extensionsSQL, displayExtensions)
import Database.PostgreSQL.Simple
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text as Text

-- Extensions

extrasExtensionsRows :: [Char] -> IO [(Maybe Text.Text, Maybe Text.Text, Maybe Text.Text, Maybe Text.Text)]
extrasExtensionsRows databaseUrl = do
  conn <- dbConnection databaseUrl
  query_ conn extensionsSQL

extrasExtensions :: [Char] -> IO ()
extrasExtensions databaseUrl = do
  rows <- extrasExtensionsRows databaseUrl
  displayExtensions rows

-- Cache hit

extrasCacheHitRows :: [Char] -> IO [(Maybe Text.Text, Maybe Text.Text)]
extrasCacheHitRows databaseUrl = do
  conn <- dbConnection databaseUrl
  query_ conn cacheHitSQL

extrasCacheHit :: [Char] -> IO ()
extrasCacheHit databaseUrl = do
  rows <- extrasCacheHitRows databaseUrl
  displayCacheHit rows

dbConnection :: [Char] -> IO Connection
dbConnection databaseUrl = do
  connectPostgreSQL $ Char8.pack databaseUrl
