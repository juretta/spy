{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Test.HUnit as H
import Data.Maybe
import Spy.Watcher
import Test.QuickCheck hiding ((.&.))
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit


test_globMatchesFile = H.assertBool
  "*.hs should match a Haskell source file"
  (matchesFile "/path/to/Main.hs" "*.hs")

test_globDoesNotMatchFile = H.assertBool
  "*.hs should not match a C source file"
  (not $ matchesFile "/path/to/Main.c" "*.hs")

test_skipEventHidden = H.assertBool
  "Skip path if hidden directory and showing hidden files is not enabled"
  (skipEvent (mockWatch {hidden = False}) "/a/b/.git/refs")

test_dontSkipEventOnMatchingGlob = H.assertBool
  "Don't skip path if the glob matches"
  (not $ skipEvent (mockWatch {glob = Just "*.hs"}) "/a/b/Main.hs")

test_skipEventOnMatchingGlob = H.assertBool
  "Skip path if the glob pattern doesn't match the file path"
  (skipEvent (mockWatch {glob = Just "*.hs"}) "/a/b/Main.sh")

test_containsHiddenPathElement = H.assertBool
  "Should identify hidden directory"
  (containsHiddenPathElement "/a/b/.git/info/exclude")

test_containsHiddenPathElementFirst = H.assertBool
  "Should identify hidden directory"
  (containsHiddenPathElement ".git/info/exclude")

test_containsNoHiddenPathElement = H.assertBool
  "Should not identify hidden directory"
  (not $ containsHiddenPathElement "info/exclude")

-- ===========================================================
mockWatch :: Spy
mockWatch = Watch { dir = "foo/bar", glob = Nothing, format = Nothing, hidden = False }

-- ===========================================================
-- Test harness

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testGroup "Watcher"
      [
        testCase "watcher/shouldMatchFilePath" test_globMatchesFile,
        testCase "watcher/shouldNotMatchFilePath" test_globDoesNotMatchFile
      ],
      testGroup "Watcher opts"
      [
        testCase "watcher/optHidden" test_skipEventHidden,
        testCase "watcher/dontSkipEventOnMatchingGlob" test_dontSkipEventOnMatchingGlob,
        testCase "watcher/skipEventOnMatchingGlob" test_skipEventOnMatchingGlob,
        testCase "watcher/containsHiddenPathElement" test_containsHiddenPathElement,
        testCase "watcher/containsHiddenPathElementFirst" test_containsHiddenPathElementFirst,
        testCase "watcher/containsNoHiddenPathElement" test_containsNoHiddenPathElement
      ]
    ]

