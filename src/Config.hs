module Config
  ( readProjectSnapshot
  , writeProjectSnapshot
  , readGlobalSnapshot
  , effectiveSnapshot
  ) where

import           Data.Char        (isSpace)
import           Data.Maybe       (fromMaybe)
import           System.Directory (XdgDirectory (..), doesFileExist,
                                   getXdgDirectory)
import           System.FilePath  ((</>))
import qualified Data.Text        as T

import           Snapshot         (SnapshotSpec (..), parseSnapshotSpec,
                                   renderSnapshotSpec)

-- | Per-project config file (commit this to VCS to pin a snapshot)
projectConfigFile :: String
projectConfigFile = ".cabal-stackage"

globalSnapshotFile :: IO FilePath
globalSnapshotFile = do
  dir <- getXdgDirectory XdgConfig "cabal-stackage"
  return $ dir </> "snapshot"

readSnapshotFile :: FilePath -> IO (Maybe SnapshotSpec)
readSnapshotFile path = do
  exists <- doesFileExist path
  if exists
    then parseSnapshotSpec . strip <$> readFile path
    else return Nothing
  where
    strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- | Read the snapshot spec from the project's .cabal-stackage file.
readProjectSnapshot :: IO (Maybe SnapshotSpec)
readProjectSnapshot = readSnapshotFile projectConfigFile

-- | Write a snapshot spec to the project's .cabal-stackage file.
writeProjectSnapshot :: SnapshotSpec -> IO ()
writeProjectSnapshot spec =
  writeFile projectConfigFile $
    T.unpack (renderSnapshotSpec spec) ++ "\n"

-- | Read the global snapshot spec from ~/.config/cabal-stackage/snapshot.
readGlobalSnapshot :: IO (Maybe SnapshotSpec)
readGlobalSnapshot = globalSnapshotFile >>= readSnapshotFile

-- | Determine the effective snapshot, in priority order:
-- CLI override > project .cabal-stackage > global config > default (lts)
effectiveSnapshot :: Maybe SnapshotSpec -> IO SnapshotSpec
effectiveSnapshot (Just spec)           = return spec
effectiveSnapshot Nothing = do
  mProj <- readProjectSnapshot
  case mProj of
    Just spec -> return spec
    Nothing   ->
      fromMaybe LtsLatest <$> readGlobalSnapshot
