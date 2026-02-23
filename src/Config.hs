module Config
  ( ProjectConfig (..)
  , emptyProjectConfig
  , parseConfigLine
  , readProjectConfig
  , writeProjectConfig
  , readProjectSnapshot
  , writeProjectSnapshot
  , readGlobalSnapshot
  , effectiveSnapshot
  , readPerSnapshotConfig
  , perSnapshotConfigFile
  , perSnapshotSuffix
  , mergeConfigs
  ) where

import           Control.Applicative ((<|>))
import           Data.List.Extra  (trim)
import           Data.Maybe       (fromMaybe, listToMaybe)
import           System.Directory (XdgDirectory (..), doesFileExist,
                                   getXdgDirectory)
import           System.FilePath  ((</>))
import qualified Data.Text        as T

import           Snapshot         (SnapshotSpec (..), constraintPkgName,
                                   parseSnapshotSpec, renderSnapshotSpec)

-- | Per-project config file (commit this to VCS to pin a snapshot)
projectConfigFile :: String
projectConfigFile = ".cabal-stackage"

-- | Project-level configuration read from .cabal-stackage.
-- All fields are optional.
--
-- Example file:
--   resolver: lts-24
--   newest: lts-24
--   oldest: lts-21
--   constraints: aeson ==2.1.0.0
data ProjectConfig = ProjectConfig
  { pcSnapshot    :: Maybe SnapshotSpec  -- ^ snapshot for build/test/etc.
  , pcNewest      :: Maybe SnapshotSpec  -- ^ newest LTS major for build-all
  , pcOldest      :: Maybe SnapshotSpec  -- ^ oldest LTS major for build-all
  , pcConstraints :: [String]            -- ^ package constraints overriding Stackage
  } deriving (Eq, Show)

emptyProjectConfig :: ProjectConfig
emptyProjectConfig = ProjectConfig Nothing Nothing Nothing []

-- | Parse one line into the config, accumulating into an existing config.
-- Accepts "key: value" pairs and, for backwards compatibility, a bare
-- snapshot spec on its own line.
parseConfigLine :: ProjectConfig -> String -> ProjectConfig
parseConfigLine cfg line =
  let s = trim line
  in if null s || listToMaybe s == Just '#'
     then cfg
     else case break (== ':') s of
       (key, ':':rest) ->
         let val = trim rest
         in case trim key of
              "resolver"    -> cfg { pcSnapshot    = parseSnapshotSpec val }
              "newest"      -> cfg { pcNewest      = parseSnapshotSpec val }
              "oldest"      -> cfg { pcOldest      = parseSnapshotSpec val }
              "constraints" -> cfg { pcConstraints = pcConstraints cfg ++ [val] }
              _             -> cfg
       -- backwards compat: bare snapshot spec with no key
       _ -> case parseSnapshotSpec s of
              Just spec -> cfg { pcSnapshot = Just spec }
              Nothing   -> cfg

readProjectConfig :: IO ProjectConfig
readProjectConfig = do
  exists <- doesFileExist projectConfigFile
  if exists
    then foldl parseConfigLine emptyProjectConfig . lines <$> readFile projectConfigFile
    else return emptyProjectConfig

writeProjectConfig :: ProjectConfig -> IO ()
writeProjectConfig cfg =
  writeFile projectConfigFile $ unlines $
    [ "resolver: "     ++ T.unpack (renderSnapshotSpec s) | Just s <- [pcSnapshot cfg] ]
    ++ [ "newest: "    ++ T.unpack (renderSnapshotSpec s) | Just s <- [pcNewest cfg] ]
    ++ [ "oldest: "    ++ T.unpack (renderSnapshotSpec s) | Just s <- [pcOldest cfg] ]
    ++ [ "constraints: " ++ c | c <- pcConstraints cfg ]

-- | Read only the snapshot field from the project config.
readProjectSnapshot :: IO (Maybe SnapshotSpec)
readProjectSnapshot = pcSnapshot <$> readProjectConfig

-- | Write the snapshot field, preserving any other fields already in the file.
writeProjectSnapshot :: SnapshotSpec -> IO ()
writeProjectSnapshot spec = do
  cfg <- readProjectConfig
  writeProjectConfig cfg { pcSnapshot = Just spec }

globalSnapshotFile :: IO FilePath
globalSnapshotFile = do
  dir <- getXdgDirectory XdgConfig "cabal-stackage"
  return $ dir </> "resolver"

readSnapshotFile :: FilePath -> IO (Maybe SnapshotSpec)
readSnapshotFile path = do
  exists <- doesFileExist path
  if exists
    then parseSnapshotSpec . trim <$> readFile path
    else return Nothing

-- | Read the global snapshot spec from ~/.config/cabal-stackage/snapshot.
readGlobalSnapshot :: IO (Maybe SnapshotSpec)
readGlobalSnapshot = globalSnapshotFile >>= readSnapshotFile

-- | Determine the effective snapshot, in priority order:
-- CLI override > project .cabal-stackage > global config > default (lts)
effectiveSnapshot :: Maybe SnapshotSpec -> IO SnapshotSpec
effectiveSnapshot (Just spec) = return spec
effectiveSnapshot Nothing = do
  mProj <- readProjectSnapshot
  case mProj of
    Just spec -> return spec
    Nothing   ->
      fromMaybe LtsLatest <$> readGlobalSnapshot

-- | Derive the per-snapshot config filename suffix from a SnapshotSpec.
-- LtsMajor 24 -> "lts24", NightlyLatest -> "nightly", LtsExact 24 31 -> "lts24.31"
-- NightlyDate d -> "nightly-" ++ d
perSnapshotSuffix :: SnapshotSpec -> String
perSnapshotSuffix LtsLatest       = "lts"
perSnapshotSuffix (LtsMajor n)    = "lts" ++ show n
perSnapshotSuffix (LtsExact m n)  = "lts" ++ show m ++ "." ++ show n
perSnapshotSuffix NightlyLatest   = "nightly"
perSnapshotSuffix (NightlyDate d) = "nightly-" ++ d

-- | The per-snapshot config file path for a given spec,
-- e.g. ".cabal-stackage.lts24" or ".cabal-stackage.nightly".
perSnapshotConfigFile :: SnapshotSpec -> FilePath
perSnapshotConfigFile spec = projectConfigFile ++ "." ++ perSnapshotSuffix spec

-- | Read a per-snapshot config file for the given spec, if it exists.
-- Returns Nothing if no such file is present.
readPerSnapshotConfig :: SnapshotSpec -> IO (Maybe ProjectConfig)
readPerSnapshotConfig spec = do
  let path = perSnapshotConfigFile spec
  exists <- doesFileExist path
  if exists
    then Just . foldl parseConfigLine emptyProjectConfig . lines <$> readFile path
    else return Nothing

-- | Merge two configs: fields from the override take precedence over base.
-- For constraints, override entries replace base entries for the same package.
mergeConfigs :: ProjectConfig -> ProjectConfig -> ProjectConfig
mergeConfigs base override = ProjectConfig
  { pcSnapshot    = pcSnapshot override <|> pcSnapshot base
  , pcNewest      = pcNewest   override <|> pcNewest   base
  , pcOldest      = pcOldest   override <|> pcOldest   base
  , pcConstraints = mergeConstraints (pcConstraints base) (pcConstraints override)
  }
  where
    mergeConstraints bs os =
      let overridePkgs = map constraintPkgName os
      in filter (\c -> constraintPkgName c `notElem` overridePkgs) bs ++ os
