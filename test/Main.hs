module Main where

import Test.HUnit
import System.Exit (exitFailure, exitSuccess)

import PGExtras (
  extrasAllLocksRows,
  extrasBloatRows,
  extrasBlockingRows,
  extrasCacheHitRows,
  extrasExtensionsRows,
  extrasIndexCacheHitRows,
  extrasIndexSizeRows,
  extrasIndexUsageRows,
  extrasKillAllRows,
  extrasLocksRows,
  extrasLongRunningQueriesRows,
  extrasMandelbrotRows,
  extrasRecordsRankRows,
  extrasSeqScansRows,
  extrasTableCacheHitRows,
  extrasTableIndexesSizeRows,
  extrasTableSizeRows,
  extrasTotalIndexSizeRows,
  extrasTotalTableSizeRows,
  extrasUnusedIndexesRows,
  extrasVacuumStatsRows)

databaseUrl :: [Char]
databaseUrl = "postgres://postgres:secret@localhost:5432/haskell-pg-extras-test"

testAllLocks :: Test
testAllLocks = TestCase ( do
  rows <- extrasAllLocksRows databaseUrl
  assertBool "AllLocks rows present" ((length rows) >= 0))

testBloat :: Test
testBloat = TestCase ( do
  rows <- extrasBloatRows databaseUrl
  assertBool "Bloat rows present" ((length rows) >= 0))

testBlocking :: Test
testBlocking = TestCase ( do
  rows <- extrasBlockingRows databaseUrl
  assertBool "Blocking rows present" ((length rows) >= 0))

testCacheHit :: Test
testCacheHit = TestCase ( do
  rows <- extrasCacheHitRows databaseUrl
  assertBool "CacheHit rows present" ((length rows) >= 0))

testExtensions :: Test
testExtensions = TestCase ( do
  rows <- extrasExtensionsRows databaseUrl
  assertBool "Extensions rows present" ((length rows) >= 0))

testIndexCacheHit :: Test
testIndexCacheHit = TestCase ( do
  rows <- extrasIndexCacheHitRows databaseUrl
  assertBool "IndexCacheHit rows present" ((length rows) >= 0))

testIndexSizeRows :: Test
testIndexSizeRows = TestCase ( do
  rows <- extrasIndexSizeRows databaseUrl
  assertBool "IndexSize rows present" ((length rows) >= 0))

testIndexUsageRows :: Test
testIndexUsageRows = TestCase ( do
  rows <- extrasIndexUsageRows databaseUrl
  assertBool "IndexUsage rows present" ((length rows) >= 0))

testKillAll :: Test
testKillAll = TestCase ( do
  rows <- extrasKillAllRows databaseUrl
  assertBool "KillAll rows present" ((length rows) >= 0))

testLocks :: Test
testLocks = TestCase ( do
  rows <- extrasLocksRows databaseUrl
  assertBool "Locks rows present" ((length rows) >= 0))

testLongRunningQueries :: Test
testLongRunningQueries = TestCase ( do
  rows <- extrasLongRunningQueriesRows databaseUrl
  assertBool "LongRunningQueries rows present" ((length rows) >= 0))

testMandelbrot :: Test
testMandelbrot = TestCase ( do
  rows <- extrasMandelbrotRows databaseUrl
  assertBool "Mandelbrot rows present" ((length rows) >= 0))

testRecordsRank :: Test
testRecordsRank = TestCase ( do
  rows <- extrasRecordsRankRows databaseUrl
  assertBool "RecordsRank rows present" ((length rows) >= 0))

testSeqScans :: Test
testSeqScans = TestCase ( do
  rows <- extrasSeqScansRows databaseUrl
  assertBool "SeqScans rows present" ((length rows) >= 0))

testIndexesSize :: Test
testIndexesSize = TestCase ( do
  rows <- extrasTableIndexesSizeRows databaseUrl
  assertBool "TableIndexesSize rows present" ((length rows) >= 0))

testTableSize :: Test
testTableSize = TestCase ( do
  rows <- extrasTableSizeRows databaseUrl
  assertBool "TableSize rows present" ((length rows) >= 0))

testTotalIndexSize :: Test
testTotalIndexSize = TestCase ( do
  rows <- extrasTotalIndexSizeRows databaseUrl
  assertBool "TotalIndexSize rows present" ((length rows) >= 0))

testTotalTableSize :: Test
testTotalTableSize = TestCase ( do
  rows <- extrasTotalTableSizeRows databaseUrl
  assertBool "TotalTableSize rows present" ((length rows) >= 0))

testUnusedIndexes :: Test
testUnusedIndexes = TestCase ( do
  rows <- extrasUnusedIndexesRows databaseUrl
  assertBool "UnusedIndexes rows present" ((length rows) >= 0))

testVacuumStats :: Test
testVacuumStats = TestCase ( do
  rows <- extrasVacuumStatsRows databaseUrl
  assertBool "VacuumStats rows present" ((length rows) >= 0))

tests = TestList [
  TestLabel "" testAllLocks,
  TestLabel "" testBloat,
  TestLabel "" testBlocking,
  TestLabel "" testCacheHit,
  TestLabel "" testExtensions,
  TestLabel "" testIndexCacheHit,
  TestLabel "" testIndexSizeRows,
  TestLabel "" testIndexUsageRows,
  TestLabel "" testKillAll,
  TestLabel "" testLocks,
  TestLabel "" testLongRunningQueries,
  TestLabel "" testMandelbrot,
  TestLabel "" testRecordsRank,
  TestLabel "" testSeqScans,
  TestLabel "" testIndexesSize,
  TestLabel "" testTableSize,
  TestLabel "" testTotalIndexSize,
  TestLabel "" testTotalTableSize,
  TestLabel "" testUnusedIndexes,
  TestLabel "" testVacuumStats]

main :: IO ()
main = do
    counts2 <- runTestTT (test [
            tests
            ])
    if (errors counts2 + failures counts2 == 0)
        then exitSuccess
        else exitFailure
