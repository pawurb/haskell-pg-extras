module Main where

import Test.HUnit
import System.Exit (exitFailure, exitSuccess)

import PGExtras (
  extrasCacheHitRows,
  extrasExtensionsRows)

databaseUrl :: [Char]
databaseUrl = "postgres://postgres:secret@localhost:5432/haskell-pg-extras-test"

testExtensions :: Test
testExtensions = TestCase ( do
  rows <- extrasExtensionsRows databaseUrl
  assertBool "Extension rows" ((length rows) > 1))

testCacheHit :: Test
testCacheHit = TestCase ( do
  rows <- extrasCacheHitRows databaseUrl
  assertBool "Cache Hit rows" ((length rows) > 1))


tests = TestList [
  TestLabel "Extensions" testExtensions,
  TestLabel "Cache hit" testCacheHit]

main :: IO ()
main = do
    counts2 <- runTestTT (test [
            tests
            ])
    if (errors counts2 + failures counts2 == 0)
        then exitSuccess
        else exitFailure
