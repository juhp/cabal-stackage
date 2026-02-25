module Main (main) where

import           Control.Monad    (filterM, forM_, when)
import           Data.List        (isSuffixOf)
import           Data.Maybe       (isJust, listToMaybe)
import qualified Data.Map.Strict  as Map
import qualified Data.Text        as T
import           SimpleCmd        (cmd_, error', needProgram, warning)
import           SimpleCmdArgs
import           System.Directory (doesDirectoryExist, doesFileExist,
                                   findExecutable, getHomeDirectory,
                                   listDirectory, withCurrentDirectory)
import           System.FilePath  ((</>))

import           Config   (ProjectConfig (..), effectiveSnapshot, mergeConfigs,
                           perSnapshotConfigFile, readPerSnapshotConfig,
                           readProjectConfig, writeProjectSnapshot)
import qualified Paths_cabal_stackage as Paths
import           Project (findProjectRoot, generateProjectFile)
import           Snapshot (SnapshotSpec (..), SnapshotsMap,
                           applyBounds, ensureCachedConfig,
                           forceRefreshConfig, getSnapshotsMap,
                           parseSnapshotSpec, readCompilerFromConfig,
                           renderSnapshotSpec, resolveSnapshot)

main :: IO ()
main = do
  needProgram "cabal"
  mroot <- findProjectRoot
  case mroot of
    Nothing -> error' "Failed to find cabal project topdir"
    Just topdir ->
      withCurrentDirectory topdir $
      simpleCmdArgs (Just Paths.version) "cabal-stackage"
        "Stack-like cabal-install wrapper using Stackage snapshots" $
        subcommands
          [ Subcommand "build"     "Build the project" $
              runCmd "build" <$> debugOpt <*> snapshotOpt <*> many (strArg "ARGS")
          , Subcommand "install"   "Install the project" $
              runCmd "install" <$> debugOpt <*> snapshotOpt <*> many (strArg "ARGS")
          , Subcommand "test"      "Test the project" $
              runCmd "test" <$> debugOpt <*> snapshotOpt <*> many (strArg "ARGS")
          , Subcommand "run"       "Run an executable" $
              runCmd "run" <$> debugOpt <*> snapshotOpt <*> many (strArg "ARGS")
          , Subcommand "repl"      "Open a REPL" $
              runCmd "repl" <$> debugOpt <*> snapshotOpt <*> many (strArg "ARGS")
          , Subcommand "haddock"   "Build documentation" $
              runCmd "haddock" <$> debugOpt <*> snapshotOpt <*> many (strArg "ARGS")
          , Subcommand "clean"     "Clean build artifacts" $
              runCmd "clean" <$> debugOpt <*> snapshotOpt <*> many (strArg "ARGS")
          , Subcommand "refresh"   "Force re-download of snapshot config" $
              refreshCmd <$> snapshotOpt
          , Subcommand "snapshot"  "Show or set the current snapshot resolver" $
              snapshotCmd <$> optional (strArg "SPEC")
          , Subcommand "build-all" "Build against multiple snapshots (stack-all equivalent)" $
              buildAllCmd <$> debugOpt <*> newestOpt <*> oldestOpt <*> many (strArg "SPEC")
          ]
      where
        debugOpt :: Parser Bool
        -- FIXME? drop Long for backcompat
        debugOpt = switchLongWith "debug" "Show snapshot, compiler, and cabal command"

        snapshotOpt :: Parser (Maybe SnapshotSpec)
        snapshotOpt =
          optional $
            optionWith (maybeReader parseSnapshotSpec) 's' "resolver" "SPEC"
              "Stackage snapshot (e.g. lts, lts-24, lts-24.31, nightly)"

        newestOpt :: Parser (Maybe SnapshotSpec)
        newestOpt =
          optional $
            optionWith (maybeReader parseSnapshotSpec) 'n' "newest" "SPEC"
              "Newest LTS major for build-all (e.g. lts-24)"

        oldestOpt :: Parser (Maybe SnapshotSpec)
        oldestOpt =
          optional $
            optionWith (maybeReader parseSnapshotSpec) 'o' "oldest" "SPEC"
              "Oldest LTS major for build-all (e.g. lts-21)"

-- | Resolve snapshot, ensure config is cached, generate project file.
-- Returns (projectFile, compilerArgs) where compilerArgs is ["-w", path]
-- if the snapshot's GHC is found on PATH, otherwise [].
setupProject :: Bool -> Maybe SnapshotSpec -> IO (FilePath, [String])
setupProject debug mSpec = do
  spec         <- effectiveSnapshot mSpec
  baseCfg      <- readProjectConfig
  mPerSnap     <- readPerSnapshotConfig spec
  when (debug && isJust mPerSnap) $
    warning $ "Per-snapshot config: " ++ perSnapshotConfigFile spec
  let merged          = maybe baseCfg (mergeConfigs baseCfg) mPerSnap
      userConstraints = pcConstraints merged
  when debug $ mapM_ (\c -> warning $ "Override: " ++ c) userConstraints
  snapshots    <- getSnapshotsMap
  pinnedId     <- either error' return $ resolveSnapshot snapshots spec
  when debug $ warning $ "Snapshot: " ++ renderSnapshotSpec spec ++ " -> " ++ pinnedId
  configPath   <- ensureCachedConfig pinnedId
  when debug $ warning $ "Config: " ++ configPath
  projectFile  <- generateProjectFile debug configPath userConstraints
  compilerArgs <- resolveCompiler debug configPath
  return (projectFile, compilerArgs)

-- | Look up the compiler named in the cabal.config on PATH,
-- falling back to Stack's program directory if not found.
-- Returns -w args to pass to cabal, or [] if the compiler wasn't found.
resolveCompiler :: Bool -> FilePath -> IO [String]
resolveCompiler debug configPath = do
  mCompiler <- readCompilerFromConfig configPath
  case mCompiler of
    Nothing       -> return []
    Just compiler -> do
      mPath <- findExecutable compiler
      case mPath of
        Just path -> do
          when debug $ warning $ "Compiler: " ++ path
          return ["-w", compiler]
        Nothing -> do
          mStackPath <- findStackGhc compiler
          case mStackPath of
            Just path -> do
              when debug $ warning $ "Compiler (stack): " ++ path
              return ["-w", path]
            Nothing -> do
              warning $ compiler ++ " not found on PATH or in ~/.stack/programs"
              return []

-- | Search for a GHC binary in Stack's program directory.
-- Stack installs GHCs at ~/.stack/programs/<arch>/<variant>/bin/ghc,
-- where <variant> may be "ghc-X.Y.Z" or "ghc-tinfo6-X.Y.Z" etc.
-- We match by version suffix to handle all naming variants.
findStackGhc :: String -> IO (Maybe FilePath)
findStackGhc compiler = do
  home <- getHomeDirectory
  let stackPrograms = home </> ".stack" </> "programs"
  exists <- doesDirectoryExist stackPrograms
  if not exists
    then return Nothing
    else do
      archs <- listDirectory stackPrograms
      -- compiler is e.g. "ghc-9.10.3"; match dirs ending with "-9.10.3"
      -- to cover variants like "ghc-tinfo6-9.10.3"
      let version = dropWhile (/= '-') compiler  -- "-9.10.3"
          matchesVersion d = version `isSuffixOf` d
      ghcDirs <- concat <$> mapM (\arch -> do
        entries <- listDirectory (stackPrograms </> arch)
        return [ stackPrograms </> arch </> d
               | d <- entries, matchesVersion d ]
        ) archs
      listToMaybe <$> filterM doesFileExist
        [ dir </> "bin" </> "ghc" | dir <- ghcDirs ]

runCmd :: String -> Bool -> Maybe SnapshotSpec -> [String] -> IO ()
runCmd cabalCmd debug mSpec extraArgs = do
  (projectFile, compilerArgs) <- setupProject debug mSpec
  let allArgs = cabalCmd : compilerArgs ++ extraArgs
      projectArg = "--project-file=" ++ projectFile
  when debug $ warning $ "cabal " ++ unwords (projectArg : allArgs)
  runCabal projectFile allArgs
  where
    -- | Run cabal with the given project file and arguments.
    runCabal :: FilePath -> [String] -> IO ()
    runCabal projectFile args =
      cmd_ "cabal" (("--project-file=" ++ projectFile) : args)

refreshCmd :: Maybe SnapshotSpec -> IO ()
refreshCmd mSpec = do
  spec       <- effectiveSnapshot mSpec
  snapshots  <- getSnapshotsMap
  pinnedId   <- either error' return $ resolveSnapshot snapshots spec
  configPath <- forceRefreshConfig pinnedId
  putStrLn $ "Refreshed: " ++ configPath

snapshotCmd :: Maybe String -> IO ()
snapshotCmd Nothing = do
  spec      <- effectiveSnapshot Nothing
  snapshots <- getSnapshotsMap
  case resolveSnapshot snapshots spec of
    Right pinnedId -> putStrLn $ "Current snapshot: " ++ pinnedId
    Left _         -> putStrLn $ "Current snapshot: " ++ renderSnapshotSpec spec
snapshotCmd (Just specStr) = do
  case parseSnapshotSpec specStr of
    Nothing   -> error' $ "Invalid snapshot spec: " ++ specStr
    Just spec -> do
      writeProjectSnapshot spec
      putStrLn $ "Snapshot set to: " ++ specStr ++ " (saved to .cabal-stackage)"

buildAllCmd :: Bool -> Maybe SnapshotSpec -> Maybe SnapshotSpec -> [String] -> IO ()
buildAllCmd debug mNewest mOldest specStrs = do
  snapshots <- getSnapshotsMap
  baseCfg <- readProjectConfig
  specs <- if null specStrs
    then do
      let newest = mNewest <|> pcNewest baseCfg
          oldest = mOldest <|> pcOldest baseCfg
      return (applyBounds newest oldest (defaultLtsSpecs snapshots))
    else mapM parseSpec specStrs
  forM_ specs $ \spec -> do
    let specStr = renderSnapshotSpec spec
    putStrLn $ "\n=== " ++ specStr ++ " ==="
    mPerSnap <- readPerSnapshotConfig spec
    when (debug && isJust mPerSnap) $
      warning $ "Per-snapshot config: " ++ perSnapshotConfigFile spec
    let merged          = maybe baseCfg (mergeConfigs baseCfg) mPerSnap
        userConstraints = pcConstraints merged
    when debug $ mapM_ (\c -> warning $ "Override: " ++ c) userConstraints
    case resolveSnapshot snapshots spec of
      Left err -> error' err
      Right pinnedId -> do
        configPath   <- ensureCachedConfig pinnedId
        projectFile  <- generateProjectFile debug configPath userConstraints
        compilerArgs <- resolveCompiler debug configPath
        let projectArg = "--project-file=" ++ projectFile
            allArgs = projectArg : "build" : compilerArgs
        when debug $ warning $ "cabal " ++ unwords allArgs
        cmd_ "cabal" allArgs
  where
    parseSpec s =
      maybe (error' $ "Invalid snapshot spec: " ++ s) return (parseSnapshotSpec s)

-- | Nightly followed by LTS majors in descending order down to lts-16.
defaultLtsSpecs :: SnapshotsMap -> [SnapshotSpec]
defaultLtsSpecs snapshots =
  NightlyLatest :
  [ LtsMajor n
  -- FIXME hardcoding
  | n <- [64,63..18]
  , Map.member (T.pack $ "lts-" ++ show n) snapshots
  ]
