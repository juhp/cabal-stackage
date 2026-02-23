module Main (main) where

import qualified Data.Map.Strict as Map
import qualified Data.Text       as T
import           Test.Tasty
import           Test.Tasty.HUnit

import           Config   (ProjectConfig (..), emptyProjectConfig,
                           mergeConfigs, parseConfigLine,
                           perSnapshotConfigFile, perSnapshotSuffix)
import           Project  (renderConstraintsBlock)
import           Snapshot (SnapshotSpec (..), applyBounds, constraintPkgName,
                           parseSnapshotSpec, renderSnapshotSpec,
                           resolveSnapshot)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "cabal-stackage"
  [ parseSnapshotSpecTests
  , renderSnapshotSpecTests
  , snapshotSpecOrdTests
  , resolveSnapshotTests
  , constraintPkgNameTests
  , parseConfigLineTests
  , emptyProjectConfigTests
  , mergeConfigsTests
  , perSnapshotSuffixTests
  , perSnapshotConfigFileTests
  , renderConstraintsBlockTests
  , applyBoundsTests
  ]

-- parseSnapshotSpec -----------------------------------------------------------

parseSnapshotSpecTests :: TestTree
parseSnapshotSpecTests = testGroup "parseSnapshotSpec"
  [ testCase "lts" $
      parseSnapshotSpec "lts" @?= Just LtsLatest
  , testCase "nightly" $
      parseSnapshotSpec "nightly" @?= Just NightlyLatest
  , testCase "lts-24 (with dash)" $
      parseSnapshotSpec "lts-24" @?= Just (LtsMajor 24)
  , testCase "lts24 (no dash)" $
      parseSnapshotSpec "lts24" @?= Just (LtsMajor 24)
  , testCase "lts-24.31" $
      parseSnapshotSpec "lts-24.31" @?= Just (LtsExact 24 31)
  , testCase "lts24.31 (no dash)" $
      parseSnapshotSpec "lts24.31" @?= Just (LtsExact 24 31)
  , testCase "nightly-2026-01-01" $
      parseSnapshotSpec "nightly-2026-01-01" @?= Just (NightlyDate "2026-01-01")
  , testCase "lts-0" $
      parseSnapshotSpec "lts-0" @?= Just (LtsMajor 0)
  , testCase "lts-1.0" $
      parseSnapshotSpec "lts-1.0" @?= Just (LtsExact 1 0)
  , testCase "empty string" $
      parseSnapshotSpec "" @?= Nothing
  , testCase "garbage" $
      parseSnapshotSpec "foobar" @?= Nothing
  , testCase "lts-" $
      parseSnapshotSpec "lts-" @?= Nothing
  , testCase "nightly-" $
      parseSnapshotSpec "nightly-" @?= Just (NightlyDate "")
  , testCase "lts-abc" $
      parseSnapshotSpec "lts-abc" @?= Nothing
  ]

-- renderSnapshotSpec ----------------------------------------------------------

renderSnapshotSpecTests :: TestTree
renderSnapshotSpecTests = testGroup "renderSnapshotSpec"
  [ testCase "LtsLatest" $
      renderSnapshotSpec LtsLatest @?= T.pack "lts"
  , testCase "LtsMajor 24" $
      renderSnapshotSpec (LtsMajor 24) @?= T.pack "lts-24"
  , testCase "LtsExact 24 31" $
      renderSnapshotSpec (LtsExact 24 31) @?= T.pack "lts-24.31"
  , testCase "NightlyLatest" $
      renderSnapshotSpec NightlyLatest @?= T.pack "nightly"
  , testCase "NightlyDate" $
      renderSnapshotSpec (NightlyDate "2026-01-01") @?= T.pack "nightly-2026-01-01"
  , testCase "round-trip LtsMajor" $
      (parseSnapshotSpec . T.unpack . renderSnapshotSpec) (LtsMajor 22) @?= Just (LtsMajor 22)
  , testCase "round-trip LtsExact" $
      (parseSnapshotSpec . T.unpack . renderSnapshotSpec) (LtsExact 22 5) @?= Just (LtsExact 22 5)
  , testCase "round-trip NightlyDate" $
      (parseSnapshotSpec . T.unpack . renderSnapshotSpec) (NightlyDate "2026-02-01")
        @?= Just (NightlyDate "2026-02-01")
  ]

-- Ord SnapshotSpec ------------------------------------------------------------

snapshotSpecOrdTests :: TestTree
snapshotSpecOrdTests = testGroup "Ord SnapshotSpec"
  [ testCase "NightlyLatest > NightlyDate" $
      assertBool "" (NightlyLatest > NightlyDate "2026-01-01")
  , testCase "NightlyLatest > LtsLatest" $
      assertBool "" (NightlyLatest > LtsLatest)
  , testCase "NightlyLatest > LtsMajor" $
      assertBool "" (NightlyLatest > LtsMajor 24)
  , testCase "NightlyDate > LtsLatest" $
      assertBool "" (NightlyDate "2026-01-01" > LtsLatest)
  , testCase "NightlyDate > LtsMajor" $
      assertBool "" (NightlyDate "2026-01-01" > LtsMajor 24)
  , testCase "NightlyDate ordering" $
      assertBool "" (NightlyDate "2026-02-01" > NightlyDate "2026-01-01")
  , testCase "LtsLatest > LtsMajor" $
      assertBool "" (LtsLatest > LtsMajor 24)
  , testCase "LtsLatest > LtsExact" $
      assertBool "" (LtsLatest > LtsExact 24 31)
  , testCase "LtsMajor > LtsExact" $
      assertBool "" (LtsMajor 24 > LtsExact 24 31)
  , testCase "LtsMajor ordering" $
      assertBool "" (LtsMajor 24 > LtsMajor 23)
  , testCase "LtsExact ordering major" $
      assertBool "" (LtsExact 24 0 > LtsExact 23 99)
  , testCase "LtsExact ordering minor" $
      assertBool "" (LtsExact 24 31 > LtsExact 24 30)
  , testCase "LtsExact equal" $
      compare (LtsExact 24 31) (LtsExact 24 31) @?= EQ
  , testCase "NightlyLatest equal" $
      compare NightlyLatest NightlyLatest @?= EQ
  , testCase "LtsLatest equal" $
      compare LtsLatest LtsLatest @?= EQ
  ]

-- resolveSnapshot -------------------------------------------------------------

resolveSnapshotTests :: TestTree
resolveSnapshotTests = testGroup "resolveSnapshot"
  [ testCase "LtsExact bypasses map" $
      resolveSnapshot Map.empty (LtsExact 24 31) @?= Right "lts-24.31"
  , testCase "NightlyDate bypasses map" $
      resolveSnapshot Map.empty (NightlyDate "2026-01-01") @?= Right "nightly-2026-01-01"
  , testCase "LtsLatest lookup" $
      resolveSnapshot testSnapshots LtsLatest @?= Right "lts-24.31"
  , testCase "LtsMajor lookup" $
      resolveSnapshot testSnapshots (LtsMajor 24) @?= Right "lts-24.31"
  , testCase "NightlyLatest lookup" $
      resolveSnapshot testSnapshots NightlyLatest @?= Right "nightly-2026-02-20"
  , testCase "missing key returns Left" $
      case resolveSnapshot Map.empty LtsLatest of
        Left _  -> return ()
        Right _ -> assertFailure "expected Left"
  ]
  where
    testSnapshots = Map.fromList
      [ (T.pack "lts",     T.pack "lts-24.31")
      , (T.pack "lts-24",  T.pack "lts-24.31")
      , (T.pack "nightly", T.pack "nightly-2026-02-20")
      ]

-- constraintPkgName -----------------------------------------------------------

constraintPkgNameTests :: TestTree
constraintPkgNameTests = testGroup "constraintPkgName"
  [ testCase "versioned constraint" $
      constraintPkgName "aeson ==2.2.3.0" @?= "aeson"
  , testCase "range constraint" $
      constraintPkgName "text >=2.1" @?= "text"
  , testCase "bare package name" $
      constraintPkgName "base" @?= "base"
  , testCase "package with hyphens" $
      constraintPkgName "some-package ==1.0" @?= "some-package"
  ]

-- parseConfigLine -------------------------------------------------------------

parseConfigLineTests :: TestTree
parseConfigLineTests = testGroup "parseConfigLine"
  [ testCase "resolver line" $
      pcSnapshot (parseConfigLine emptyProjectConfig "resolver: lts-24")
        @?= Just (LtsMajor 24)
  , testCase "newest line" $
      pcNewest (parseConfigLine emptyProjectConfig "newest: lts-24")
        @?= Just (LtsMajor 24)
  , testCase "oldest line" $
      pcOldest (parseConfigLine emptyProjectConfig "oldest: lts-21")
        @?= Just (LtsMajor 21)
  , testCase "constraints line" $
      pcConstraints (parseConfigLine emptyProjectConfig "constraints: aeson ==2.1.0.0")
        @?= ["aeson ==2.1.0.0"]
  , testCase "constraints accumulate" $
      let cfg1 = parseConfigLine emptyProjectConfig "constraints: aeson ==2.1.0.0"
          cfg2 = parseConfigLine cfg1 "constraints: text >=2.1"
      in pcConstraints cfg2 @?= ["aeson ==2.1.0.0", "text >=2.1"]
  , testCase "comment line ignored" $
      parseConfigLine emptyProjectConfig "# this is a comment" @?= emptyProjectConfig
  , testCase "blank line ignored" $
      parseConfigLine emptyProjectConfig "" @?= emptyProjectConfig
  , testCase "whitespace-only line ignored" $
      parseConfigLine emptyProjectConfig "   " @?= emptyProjectConfig
  , testCase "bare snapshot spec (backwards compat)" $
      pcSnapshot (parseConfigLine emptyProjectConfig "lts-24")
        @?= Just (LtsMajor 24)
  , testCase "unknown key ignored" $
      parseConfigLine emptyProjectConfig "foobar: baz" @?= emptyProjectConfig
  ]

-- emptyProjectConfig ----------------------------------------------------------

emptyProjectConfigTests :: TestTree
emptyProjectConfigTests = testGroup "emptyProjectConfig"
  [ testCase "snapshot is Nothing" $
      pcSnapshot emptyProjectConfig @?= Nothing
  , testCase "newest is Nothing" $
      pcNewest emptyProjectConfig @?= Nothing
  , testCase "oldest is Nothing" $
      pcOldest emptyProjectConfig @?= Nothing
  , testCase "constraints is empty" $
      pcConstraints emptyProjectConfig @?= []
  ]

-- mergeConfigs ----------------------------------------------------------------

mergeConfigsTests :: TestTree
mergeConfigsTests = testGroup "mergeConfigs"
  [ testCase "override snapshot takes precedence" $
      let base = emptyProjectConfig { pcSnapshot = Just LtsLatest }
          over = emptyProjectConfig { pcSnapshot = Just (LtsMajor 23) }
      in pcSnapshot (mergeConfigs base over) @?= Just (LtsMajor 23)
  , testCase "base snapshot used when override is Nothing" $
      let base = emptyProjectConfig { pcSnapshot = Just LtsLatest }
      in pcSnapshot (mergeConfigs base emptyProjectConfig) @?= Just LtsLatest
  , testCase "override newest takes precedence" $
      let base = emptyProjectConfig { pcNewest = Just (LtsMajor 24) }
          over = emptyProjectConfig { pcNewest = Just (LtsMajor 23) }
      in pcNewest (mergeConfigs base over) @?= Just (LtsMajor 23)
  , testCase "override oldest takes precedence" $
      let base = emptyProjectConfig { pcOldest = Just (LtsMajor 18) }
          over = emptyProjectConfig { pcOldest = Just (LtsMajor 20) }
      in pcOldest (mergeConfigs base over) @?= Just (LtsMajor 20)
  , testCase "constraints merged, override replaces same pkg" $
      let base = emptyProjectConfig { pcConstraints = ["aeson ==2.0", "text ==2.1"] }
          over = emptyProjectConfig { pcConstraints = ["aeson ==2.2"] }
      in pcConstraints (mergeConfigs base over) @?= ["text ==2.1", "aeson ==2.2"]
  , testCase "constraints from both when no overlap" $
      let base = emptyProjectConfig { pcConstraints = ["aeson ==2.0"] }
          over = emptyProjectConfig { pcConstraints = ["text ==2.1"] }
      in pcConstraints (mergeConfigs base over) @?= ["aeson ==2.0", "text ==2.1"]
  , testCase "empty base with override" $
      let over = emptyProjectConfig { pcSnapshot = Just (LtsMajor 22) }
      in mergeConfigs emptyProjectConfig over @?= over
  , testCase "base with empty override" $
      let base = emptyProjectConfig { pcSnapshot = Just (LtsMajor 22) }
      in mergeConfigs base emptyProjectConfig @?= base
  ]

-- perSnapshotSuffix -----------------------------------------------------------

perSnapshotSuffixTests :: TestTree
perSnapshotSuffixTests = testGroup "perSnapshotSuffix"
  [ testCase "LtsLatest" $
      perSnapshotSuffix LtsLatest @?= "lts"
  , testCase "LtsMajor 24" $
      perSnapshotSuffix (LtsMajor 24) @?= "lts24"
  , testCase "LtsExact 24 31" $
      perSnapshotSuffix (LtsExact 24 31) @?= "lts24.31"
  , testCase "NightlyLatest" $
      perSnapshotSuffix NightlyLatest @?= "nightly"
  , testCase "NightlyDate" $
      perSnapshotSuffix (NightlyDate "2026-01-01") @?= "nightly-2026-01-01"
  ]

-- perSnapshotConfigFile -------------------------------------------------------

perSnapshotConfigFileTests :: TestTree
perSnapshotConfigFileTests = testGroup "perSnapshotConfigFile"
  [ testCase "LtsMajor 24" $
      perSnapshotConfigFile (LtsMajor 24) @?= ".cabal-stackage.lts24"
  , testCase "NightlyLatest" $
      perSnapshotConfigFile NightlyLatest @?= ".cabal-stackage.nightly"
  , testCase "LtsExact 24 31" $
      perSnapshotConfigFile (LtsExact 24 31) @?= ".cabal-stackage.lts24.31"
  , testCase "NightlyDate" $
      perSnapshotConfigFile (NightlyDate "2026-01-01") @?= ".cabal-stackage.nightly-2026-01-01"
  ]

-- renderConstraintsBlock ------------------------------------------------------

renderConstraintsBlockTests :: TestTree
renderConstraintsBlockTests = testGroup "renderConstraintsBlock"
  [ testCase "empty list" $
      renderConstraintsBlock [] @?= []
  , testCase "single constraint" $
      renderConstraintsBlock ["aeson ==2.0"] @?= ["constraints: aeson ==2.0"]
  , testCase "two constraints" $
      renderConstraintsBlock ["aeson ==2.0", "text ==2.1"]
        @?= [ "constraints: aeson ==2.0,"
             , "             text ==2.1"
             ]
  , testCase "three constraints" $
      renderConstraintsBlock ["aeson ==2.0", "text ==2.1", "base <5"]
        @?= [ "constraints: aeson ==2.0,"
             , "             text ==2.1,"
             , "             base <5"
             ]
  ]

-- applyBounds -----------------------------------------------------------------

applyBoundsTests :: TestTree
applyBoundsTests = testGroup "applyBounds"
  [ testCase "no bounds" $
      applyBounds Nothing Nothing specs @?= specs
  , testCase "newest only" $
      applyBounds (Just (LtsMajor 23)) Nothing specs
        @?= [LtsMajor 23, LtsMajor 22, LtsMajor 21]
  , testCase "oldest only" $
      applyBounds Nothing (Just (LtsMajor 23)) specs
        @?= [NightlyLatest, LtsMajor 24, LtsMajor 23]
  , testCase "both bounds" $
      applyBounds (Just (LtsMajor 23)) (Just (LtsMajor 22)) specs
        @?= [LtsMajor 23, LtsMajor 22]
  , testCase "bounds excluding everything" $
      applyBounds (Just (LtsMajor 20)) (Just (LtsMajor 25)) specs @?= []
  , testCase "newest LtsMajor filters nightly too" $
      applyBounds (Just (LtsMajor 23)) Nothing [NightlyLatest, LtsMajor 24, LtsMajor 23]
        @?= [LtsMajor 23]
  ]
  where
    specs = [NightlyLatest, LtsMajor 24, LtsMajor 23, LtsMajor 22, LtsMajor 21]
