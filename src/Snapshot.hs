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
  , applyBounds
  , getMajorVers
  ) where

import           Control.Monad              (unless)
import qualified Data.ByteString            as B
import           Control.Applicative        ((<|>))
import           Data.List                  (isPrefixOf, sortBy, stripPrefix)
import           Data.Map.Strict            (Map)
import           Data.Maybe                 (listToMaybe, mapMaybe)
import qualified Data.Map.Strict            as Map
import           Data.Ord                   (comparing, Down(Down))
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

isLtsMajor :: SnapshotSpec -> Bool
isLtsMajor (LtsMajor _) = True
isLtsMajor _ = False

instance Ord SnapshotSpec where
  compare s1 s2 =
    case (s1,s2) of
      (NightlyLatest,NightlyLatest) -> EQ
      (NightlyLatest,_) -> GT
      (_,NightlyLatest) -> LT
      (NightlyDate d1,NightlyDate d2) -> compare d1 d2
      (NightlyDate _, _) -> GT
      (_, NightlyDate _) -> LT
      (LtsLatest,LtsLatest) -> EQ
      (LtsLatest,_) -> GT
      (_,LtsLatest) -> LT
      (LtsMajor m, LtsMajor n) -> compare m n
      (LtsMajor _, _) -> GT
      (_, LtsMajor _) -> LT
      (LtsExact m1 n1, LtsExact m2 n2) ->
        let cmp = compare m1 m2
        in if cmp == EQ
           then compare n1 n2
           else cmp

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

renderSnapshotSpec :: SnapshotSpec -> String
renderSnapshotSpec LtsLatest       = "lts"
renderSnapshotSpec (LtsMajor n)    = "lts-" ++ show n
renderSnapshotSpec (LtsExact m n)  = "lts-" ++ show m ++ "." ++ show n
renderSnapshotSpec NightlyLatest   =  "nightly"
renderSnapshotSpec (NightlyDate d) =  "nightly-" ++ d

-- | Decoded snapshots.json: alias -> pinned snapshot id
-- e.g. "lts" -> "lts-24.31", "lts-24" -> "lts-24.31"
type SnapshotsMap = Map Text Text

-- | Fetch snapshots.json, cached for 200 minutes
getSnapshotsMap :: IO SnapshotsMap
getSnapshotsMap =
  let url = "https://www.stackage.org/download/snapshots.json"
  in getCachedJSON "stackage-snapshots" "snapshots.json" url 200

-- | Resolve a snapshot spec to a pinned snapshot ID (e.g. "lts-24.31").
-- Exact specs (LtsExact, NightlyDate) are returned directly without a lookup.
resolveSnapshot :: SnapshotsMap -> SnapshotSpec -> Either String String
resolveSnapshot _ (LtsExact m n)  = Right $ "lts-" ++ show m ++ "." ++ show n
resolveSnapshot _ (NightlyDate d) = Right $ "nightly-" ++ d
resolveSnapshot snapshots spec =
  let key = renderSnapshotSpec spec
  in case Map.lookup (T.pack key) snapshots of
    Just pinned -> Right (T.unpack pinned)
    Nothing     -> Left $ "Snapshot not found in snapshots.json: " ++ key

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
  let mwithcompiler = listToMaybe $ dropWhile (\l -> null l || "--" `isPrefixOf` l) ls
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

-- | Filter snapshots within given newest/oldest LTS major bounds.
applyBounds :: Maybe SnapshotSpec -> Maybe SnapshotSpec -> [SnapshotSpec]
            -> [SnapshotSpec]
applyBounds mNewest mOldest = filter inRange
  where
    inRange spec =
      maybe True (spec <=) mNewest && maybe True (spec >=) mOldest

getMajorVers :: SnapshotsMap -> [SnapshotSpec]
getMajorVers snapshots =
  sortBy (comparing Down) $
  filter isLtsMajor $
  mapMaybe (parseSnapshotSpec . T.unpack) $ Map.keys snapshots
