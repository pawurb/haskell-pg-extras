{-# LANGUAGE OverloadedStrings #-}

module PGExtras (
  extrasAllLocksRows,
  extrasAllLocks,
  extrasBloatRows,
  extrasBloat,
  extrasBlockingRows,
  extrasBlocking,
  extrasCacheHitRows,
  extrasCacheHit,
  extrasCallsRows,
  extrasCalls,
  extrasExtensionsRows,
  extrasExtensions,
  extrasIndexCacheHitRows,
  extrasIndexCacheHit,
  extrasIndexSizeRows,
  extrasIndexSize,
  extrasIndexUsageRows,
  extrasIndexUsage,
  extrasKillAllRows,
  extrasKillAll,
  extrasLocksRows,
  extrasLocks,
  extrasLongRunningQueriesRows,
  extrasLongRunningQueries,
  extrasMandelbrotRows,
  extrasMandelbrot,
  extrasRecordsRankRows,
  extrasRecordsRank,
  extrasSeqScansRows,
  extrasSeqScans,
  extrasTableCacheHitRows,
  extrasTableCacheHit,
  extrasTableIndexesSizeRows,
  extrasTableIndexesSize,
  extrasTableSizeRows,
  extrasTableSize,
  extrasTotalIndexSizeRows,
  extrasTotalIndexSize,
  extrasTotalTableSizeRows,
  extrasTotalTableSize,
  extrasUnusedIndexesRows,
  extrasUnusedIndexes,
  extrasVacuumStatsRows,
  extrasVacuumStats
) where

import Database.PostgreSQL.Simple
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text as Text
import Data.Time (ZonedTime)

import PGExtras.Queries.AllLocks (allLocksSQL, displayAllLocks)
import PGExtras.Queries.Bloat (bloatSQL, displayBloat)
import PGExtras.Queries.Blocking (blockingSQL, displayBlocking)
import PGExtras.Queries.CacheHit (cacheHitSQL, displayCacheHit)
import PGExtras.Queries.Calls (callsSQL, displayCalls)
import PGExtras.Queries.Extensions (extensionsSQL, displayExtensions)
import PGExtras.Queries.IndexCacheHit (indexCacheHitSQL, displayIndexCacheHit)
import PGExtras.Queries.IndexSize (indexSizeSQL, displayIndexSize)
import PGExtras.Queries.IndexUsage (indexUsageSQL, displayIndexUsage)
import PGExtras.Queries.KillAll (killAllSQL, displayKillAll)
import PGExtras.Queries.Locks (locksSQL, displayLocks)
import PGExtras.Queries.LongRunningQueries (longRunningQueriesSQL, displayLongRunningQueries)
import PGExtras.Queries.Mandelbrot (mandelbrotSQL, displayMandelbrot)
import PGExtras.Queries.RecordsRank (recordsRankSQL, displayRecordsRank)
import PGExtras.Queries.SeqScans (seqScansSQL, displaySeqScans)
import PGExtras.Queries.TableCacheHit (tableCacheHitSQL, displayTableCacheHit)
import PGExtras.Queries.TableIndexesSize (tableIndexesSizeSQL, displayTableIndexesSize)
import PGExtras.Queries.TableSize (tableSizeSQL, displayTableSize)
import PGExtras.Queries.TotalIndexSize (totalIndexSizeSQL, displayTotalIndexSize)
import PGExtras.Queries.TotalTableSize (totalTableSizeSQL, displayTotalTableSize)
import PGExtras.Queries.UnusedIndexes (unusedIndexesSQL, displayUnusedIndexes)
import PGExtras.Queries.VacuumStats (vacuumStatsSQL, displayVacuumStats)

-- AllLocks

extrasAllLocksRows :: [Char] -> IO [(Maybe Int, Maybe Text.Text, Maybe Text.Text, Maybe Bool, Maybe Text.Text, Maybe Text.Text, Maybe ZonedTime)]
extrasAllLocksRows databaseUrl = do
  conn <- dbConnection databaseUrl
  query_ conn allLocksSQL

extrasAllLocks :: [Char] -> IO ()
extrasAllLocks databaseUrl = do
  rows <- extrasAllLocksRows databaseUrl
  displayAllLocks rows

-- Bloat

extrasBloatRows :: [Char] -> IO [(Maybe Text.Text, Maybe Text.Text, Maybe Text.Text, Maybe Rational, Maybe Text.Text)]
extrasBloatRows databaseUrl = do
  conn <- dbConnection databaseUrl
  query_ conn bloatSQL

extrasBloat :: [Char] -> IO ()
extrasBloat databaseUrl = do
  rows <- extrasBloatRows databaseUrl
  displayBloat rows

-- Blocking

extrasBlockingRows :: [Char] -> IO [(Maybe Text.Text, Maybe Text.Text, Maybe Text.Text, Maybe Text.Text, Maybe Text.Text, Maybe Text.Text)]
extrasBlockingRows databaseUrl = do
  conn <- dbConnection databaseUrl
  query_ conn blockingSQL

extrasBlocking :: [Char] -> IO ()
extrasBlocking databaseUrl = do
  rows <- extrasBlockingRows databaseUrl
  displayBlocking rows

-- CacheHit

extrasCacheHitRows :: [Char] -> IO [(Maybe Text.Text, Maybe Text.Text)]
extrasCacheHitRows databaseUrl = do
  conn <- dbConnection databaseUrl
  query_ conn cacheHitSQL

extrasCacheHit :: [Char] -> IO ()
extrasCacheHit databaseUrl = do
  rows <- extrasCacheHitRows databaseUrl
  displayCacheHit rows

-- Calls

extrasCallsRows :: [Char] -> IO [(Maybe Text.Text, Maybe Text.Text, Maybe Text.Text, Maybe Text.Text, Maybe Text.Text)]
extrasCallsRows databaseUrl = do
  conn <- dbConnection databaseUrl
  query_ conn callsSQL

extrasCalls :: [Char] -> IO ()
extrasCalls databaseUrl = do
  rows <- extrasCallsRows databaseUrl
  displayCalls rows

-- Extensions

extrasExtensionsRows :: [Char] -> IO [(Maybe Text.Text, Maybe Text.Text, Maybe Text.Text, Maybe Text.Text)]
extrasExtensionsRows databaseUrl = do
  conn <- dbConnection databaseUrl
  query_ conn extensionsSQL

extrasExtensions :: [Char] -> IO ()

extrasExtensions databaseUrl = do
  rows <- extrasExtensionsRows databaseUrl
  displayExtensions rows

-- IndexCacheHit

extrasIndexCacheHitRows :: [Char] -> IO [(Maybe Text.Text, Maybe Int, Maybe Int, Maybe Int, Maybe Text.Text)]
extrasIndexCacheHitRows databaseUrl = do
  conn <- dbConnection databaseUrl
  query_ conn indexCacheHitSQL

extrasIndexCacheHit :: [Char] -> IO ()
extrasIndexCacheHit databaseUrl = do
  rows <- extrasIndexCacheHitRows databaseUrl
  displayIndexCacheHit rows

-- IndexSize

extrasIndexSizeRows :: [Char] -> IO [(Maybe Text.Text, Maybe Text.Text)]
extrasIndexSizeRows databaseUrl = do
  conn <- dbConnection databaseUrl
  query_ conn indexSizeSQL

extrasIndexSize :: [Char] -> IO ()
extrasIndexSize databaseUrl = do
  rows <- extrasIndexSizeRows databaseUrl
  displayIndexSize rows

-- IndexUsage

extrasIndexUsageRows :: [Char] -> IO [(Maybe Text.Text, Maybe Text.Text, Maybe Int)]
extrasIndexUsageRows databaseUrl = do
  conn <- dbConnection databaseUrl
  query_ conn indexUsageSQL

extrasIndexUsage :: [Char] -> IO ()
extrasIndexUsage databaseUrl = do
  rows <- extrasIndexUsageRows databaseUrl
  displayIndexUsage rows

-- KillAll

extrasKillAllRows :: [Char] -> IO [(Maybe Bool, Maybe Text.Text)]
extrasKillAllRows databaseUrl = do
  conn <- dbConnection databaseUrl
  query_ conn killAllSQL

extrasKillAll :: [Char] -> IO ()
extrasKillAll databaseUrl = do
  rows <- extrasKillAllRows databaseUrl
  displayKillAll rows

-- Locks

extrasLocksRows :: [Char] -> IO [(Maybe Int, Maybe Text.Text, Maybe Text.Text, Maybe Bool, Maybe Text.Text, Maybe Text.Text, Maybe ZonedTime)]
extrasLocksRows databaseUrl = do
  conn <- dbConnection databaseUrl
  query_ conn locksSQL

extrasLocks :: [Char] -> IO ()
extrasLocks databaseUrl = do
  rows <- extrasLocksRows databaseUrl
  displayLocks rows

-- LongRunningQueries

extrasLongRunningQueriesRows :: [Char] -> IO [(Maybe Int, Maybe ZonedTime, Maybe Text.Text)]
extrasLongRunningQueriesRows databaseUrl = do
  conn <- dbConnection databaseUrl
  query_ conn longRunningQueriesSQL

extrasLongRunningQueries :: [Char] -> IO ()
extrasLongRunningQueries databaseUrl = do
  rows <- extrasLongRunningQueriesRows databaseUrl
  displayLongRunningQueries rows

-- Mandelbrot

extrasMandelbrotRows :: [Char] -> IO [(Maybe Text.Text, Maybe Text.Text)]
extrasMandelbrotRows databaseUrl = do
  conn <- dbConnection databaseUrl
  query_ conn mandelbrotSQL

extrasMandelbrot :: [Char] -> IO ()
extrasMandelbrot databaseUrl = do
  rows <- extrasMandelbrotRows databaseUrl
  displayMandelbrot rows

-- RecordsRank

extrasRecordsRankRows :: [Char] -> IO [(Maybe Text.Text, Maybe Int)]
extrasRecordsRankRows databaseUrl = do
  conn <- dbConnection databaseUrl
  query_ conn recordsRankSQL

extrasRecordsRank :: [Char] -> IO ()
extrasRecordsRank databaseUrl = do
  rows <- extrasRecordsRankRows databaseUrl
  displayRecordsRank rows

-- SeqScans

extrasSeqScansRows :: [Char] -> IO [(Maybe Text.Text, Maybe Int)]
extrasSeqScansRows databaseUrl = do
  conn <- dbConnection databaseUrl
  query_ conn seqScansSQL

extrasSeqScans :: [Char] -> IO ()
extrasSeqScans databaseUrl = do
  rows <- extrasSeqScansRows databaseUrl
  displaySeqScans rows

-- TableCacheHit

extrasTableCacheHitRows :: [Char] -> IO [(Maybe Text.Text, Maybe Int, Maybe Int, Maybe Int, Maybe Text.Text)]
extrasTableCacheHitRows databaseUrl = do
  conn <- dbConnection databaseUrl
  query_ conn tableCacheHitSQL

extrasTableCacheHit :: [Char] -> IO ()
extrasTableCacheHit databaseUrl = do
  rows <- extrasTableCacheHitRows databaseUrl
  displayTableCacheHit rows

-- TableIndexesSize

extrasTableIndexesSizeRows :: [Char] -> IO [(Maybe Text.Text, Maybe Text.Text)]
extrasTableIndexesSizeRows databaseUrl = do
  conn <- dbConnection databaseUrl
  query_ conn tableIndexesSizeSQL

extrasTableIndexesSize :: [Char] -> IO ()
extrasTableIndexesSize databaseUrl = do
  rows <- extrasTableIndexesSizeRows databaseUrl
  displayTableIndexesSize rows

-- TableSize

extrasTableSizeRows :: [Char] -> IO [(Maybe Text.Text, Maybe Text.Text)]
extrasTableSizeRows databaseUrl = do
  conn <- dbConnection databaseUrl
  query_ conn tableSizeSQL

extrasTableSize :: [Char] -> IO ()
extrasTableSize databaseUrl = do
  rows <- extrasTableSizeRows databaseUrl
  displayTableSize rows

-- TotalIndexSize

extrasTotalIndexSizeRows :: [Char] -> IO [(Maybe Text.Text, Maybe Text.Text)]
extrasTotalIndexSizeRows databaseUrl = do
  conn <- dbConnection databaseUrl
  query_ conn totalIndexSizeSQL

extrasTotalIndexSize :: [Char] -> IO ()
extrasTotalIndexSize databaseUrl = do
  rows <- extrasTotalIndexSizeRows databaseUrl
  displayTotalIndexSize rows

-- TotalTableSize

extrasTotalTableSizeRows :: [Char] -> IO [(Maybe Text.Text, Maybe Text.Text)]
extrasTotalTableSizeRows databaseUrl = do
  conn <- dbConnection databaseUrl
  query_ conn totalTableSizeSQL

extrasTotalTableSize :: [Char] -> IO ()
extrasTotalTableSize databaseUrl = do
  rows <- extrasTotalTableSizeRows databaseUrl
  displayTotalTableSize rows

-- UnusedIndexes

extrasUnusedIndexesRows :: [Char] -> IO [(Maybe Text.Text, Maybe Text.Text, Maybe Text.Text, Maybe Int)]
extrasUnusedIndexesRows databaseUrl = do
  conn <- dbConnection databaseUrl
  query_ conn unusedIndexesSQL

extrasUnusedIndexes :: [Char] -> IO ()
extrasUnusedIndexes databaseUrl = do
  rows <- extrasUnusedIndexesRows databaseUrl
  displayUnusedIndexes rows

-- VacuumStats

extrasVacuumStatsRows :: [Char] -> IO [(Maybe Text.Text, Maybe Text.Text, Maybe Text.Text, Maybe Text.Text, Maybe Text.Text, Maybe Text.Text, Maybe Text.Text, Maybe Text.Text)]
extrasVacuumStatsRows databaseUrl = do
  conn <- dbConnection databaseUrl
  query_ conn vacuumStatsSQL

extrasVacuumStats :: [Char] -> IO ()
extrasVacuumStats databaseUrl = do
  rows <- extrasVacuumStatsRows databaseUrl
  displayVacuumStats rows

-- Other

dbConnection :: [Char] -> IO Connection
dbConnection databaseUrl = do
  connectPostgreSQL $ Char8.pack databaseUrl
