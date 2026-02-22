module Snapshot
  ( SnapshotSpec (..)
  , SnapshotsMap
  , parseSnapshotSpec
  , renderSnapshotSpec
  , getSnapshotsMap
  , resolveSnapshot
  , ensureCachedConfig
  , forceRefreshConfig
  , readCompilerFromConfig
  , constraintPkgName
  , readConfigConstraints
  ) where

import           Control.Monad              (unless)
import qualified Data.ByteString            as B
import           Control.Applicative        ((<|>))
import           Data.List                  (isPrefixOf, stripPrefix)
import           Data.Map.Strict            (Map)
import           Data.Maybe                 (listToMaybe)
import qualified Data.Map.Strict            as Map
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Network.HTTP.Simple        (getResponseBody, httpBS,
                                             parseRequest_)
import           System.Cached.JSON         (getCachedJSON)
import           System.Directory           (XdgDirectory (..),
                                             createDirectoryIfMissing,
                                             doesFileExist, getXdgDirectory)
import           System.FilePath            ((</>))
import           Text.Read                  (readMaybe)

data SnapshotSpec
  = LtsLatest
  | LtsMajor Int
  | LtsExact Int Int
  | NightlyLatest
  | NightlyDate String
  deriving (Show, Eq)

parseSnapshotSpec :: String -> Maybe SnapshotSpec
parseSnapshotSpec "lts"     = Just LtsLatest
parseSnapshotSpec "nightly" = Just NightlyLatest
parseSnapshotSpec s =
  case stripPrefix "lts-" s <|> stripPrefix "lts" s of
    Just rest ->
      case break (== '.') rest of
        (major, "")        -> LtsMajor <$> readMaybe major
        (major, '.':minor) -> LtsExact <$> readMaybe major <*> readMaybe minor
        _                  -> Nothing
    Nothing ->
      NightlyDate <$> stripPrefix "nightly-" s

renderSnapshotSpec :: SnapshotSpec -> Text
renderSnapshotSpec LtsLatest       = T.pack "lts"
renderSnapshotSpec (LtsMajor n)    = T.pack $ "lts-" ++ show n
renderSnapshotSpec (LtsExact m n)  = T.pack $ "lts-" ++ show m ++ "." ++ show n
renderSnapshotSpec NightlyLatest   = T.pack "nightly"
renderSnapshotSpec (NightlyDate d) = T.pack $ "nightly-" ++ d

-- | Decoded snapshots.json: alias -> pinned snapshot id
-- e.g. "lts" -> "lts-24.31", "lts-24" -> "lts-24.31"
type SnapshotsMap = Map Text Text

snapshotsJsonUrl :: String
snapshotsJsonUrl = "https://www.stackage.org/download/snapshots.json"

-- | Fetch snapshots.json, cached for 60 minutes
getSnapshotsMap :: IO SnapshotsMap
getSnapshotsMap =
  getCachedJSON "stackage-snapshots" "snapshots.json" snapshotsJsonUrl 200

-- | Resolve a snapshot spec to a pinned snapshot ID (e.g. "lts-24.31").
-- Exact specs (LtsExact, NightlyDate) are returned directly without a lookup.
resolveSnapshot :: SnapshotsMap -> SnapshotSpec -> Either String String
resolveSnapshot _ (LtsExact m n)  = Right $ "lts-" ++ show m ++ "." ++ show n
resolveSnapshot _ (NightlyDate d) = Right $ "nightly-" ++ d
resolveSnapshot snapshots spec =
  let key = renderSnapshotSpec spec
  in case Map.lookup key snapshots of
    Just pinned -> Right (T.unpack pinned)
    Nothing     -> Left $ "Snapshot not found in snapshots.json: " ++ T.unpack key

cacheDir :: IO FilePath
cacheDir = getXdgDirectory XdgCache "cabal-stackage"

configUrl :: String -> String
configUrl snapId = "https://www.stackage.org/" ++ snapId ++ "/cabal.config"

downloadConfig :: String -> FilePath -> IO ()
downloadConfig snapId dest = do
  let url = configUrl snapId
  putStrLn $ "Fetching " ++ url
  response <- httpBS (parseRequest_ url)
  B.writeFile dest (getResponseBody response)

-- | Return the cached cabal.config path for a pinned snapshot ID,
-- downloading it first if not already present.
ensureCachedConfig :: String -> IO FilePath
ensureCachedConfig snapId = do
  dir <- cacheDir
  createDirectoryIfMissing True dir
  let dest = dir </> snapId ++ ".config"
  exists <- doesFileExist dest
  unless exists $ downloadConfig snapId dest
  return dest

-- | Read the 'with-compiler:' value from a cached cabal.config,
-- e.g. "ghc-9.10.3".
readCompilerFromConfig :: FilePath -> IO (Maybe String)
readCompilerFromConfig configPath = do
  ls <- lines <$> readFile configPath
  let mwithcompiler = listToMaybe $ dropWhile ("--" `isPrefixOf`) ls
  return $ stripPrefix "with-compiler: " =<< mwithcompiler

-- | Re-download the cabal.config for a pinned snapshot ID unconditionally.
forceRefreshConfig :: String -> IO FilePath
forceRefreshConfig snapId = do
  dir <- cacheDir
  createDirectoryIfMissing True dir
  let dest = dir </> snapId ++ ".config"
  downloadConfig snapId dest
  return dest

-- | Extract the package name from a constraint string.
-- e.g. "aeson ==2.2.3.0" -> "aeson"
constraintPkgName :: String -> String
constraintPkgName = takeWhile (/= ' ')

-- | Read all constraint entries from a Stackage cabal.config.
-- Returns a list of strings like ["aeson ==2.2.3.0", "text ==2.1.1", ...]
readConfigConstraints :: FilePath -> IO [String]
readConfigConstraints configPath = do
  ls <- lines <$> readFile configPath
  case break (isPrefixOf "constraints:") ls of
    (_, []) -> return []
    (_, first:rest) ->
      let firstEntry  = dropWhile (== ' ') (drop (length "constraints:") first)
          contLines   = takeWhile startsWithSpace rest
          allEntries  = firstEntry : map (dropWhile (== ' ')) contLines
      in return $ filter (not . null) $ map stripComma allEntries
  where
    startsWithSpace []    = False
    startsWithSpace (c:_) = c == ' '
    stripComma s
      | not (null s) && last s == ',' = init s
      | otherwise                     = s
