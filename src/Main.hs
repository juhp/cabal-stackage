module Main (main) where

import           Control.Monad    (forM_)
import qualified Data.Map.Strict  as Map
import qualified Data.Text        as T
import           SimpleCmd        (cmd_, error', needProgram)
import           SimpleCmdArgs
import           System.Directory (withCurrentDirectory)

import           Config
import           Project
import           Snapshot

main :: IO ()
main = do
  needProgram "cabal"
  mroot <- findProjectRoot
  case mroot of
    Nothing -> error' "Failed to find cabal project topdir"
    Just topdir ->
      withCurrentDirectory topdir $
      simpleCmdArgs Nothing "cabal-stackage"
        "Stack-like cabal-install wrapper using Stackage snapshots" $
        subcommands
          [ Subcommand "build"     "Build the project" $
              passthroughCmd "build" <$> snapshotOpt <*> many (strArg "ARGS")
          , Subcommand "install"   "Install the project" $
              passthroughCmd "install" <$> snapshotOpt <*> many (strArg "ARGS")
          , Subcommand "test"      "Test the project" $
              passthroughCmd "test" <$> snapshotOpt <*> many (strArg "ARGS")
          , Subcommand "run"       "Run an executable" $
              passthroughCmd "run" <$> snapshotOpt <*> many (strArg "ARGS")
          , Subcommand "repl"      "Open a REPL" $
              passthroughCmd "repl" <$> snapshotOpt <*> many (strArg "ARGS")
          , Subcommand "haddock"   "Build documentation" $
              passthroughCmd "haddock" <$> snapshotOpt <*> many (strArg "ARGS")
          , Subcommand "clean"     "Clean build artifacts" $
              passthroughCmd "clean" <$> snapshotOpt <*> many (strArg "ARGS")
          , Subcommand "refresh"   "Force re-download of snapshot config" $
              refreshCmd <$> snapshotOpt
          , Subcommand "snapshot"  "Show or set the current snapshot" $
              snapshotCmd <$> optional (strArg "SPEC")
          , Subcommand "build-all" "Build against multiple snapshots (stack-all equivalent)" $
              buildAllCmd <$> many (strArg "SPEC")
          ]
      where
        snapshotOpt :: Parser (Maybe SnapshotSpec)
        snapshotOpt =
          optional $
            optionWith (maybeReader parseSnapshotSpec) 's' "snapshot" "SPEC"
              "Stackage snapshot (e.g. lts, lts-24, lts-24.31, nightly)"

-- | Resolve snapshot, ensure config is cached, generate project file.
-- Returns (projectRoot, projectFile).
setupProject :: Maybe SnapshotSpec -> IO FilePath
setupProject mSpec = do
  spec        <- effectiveSnapshot mSpec
  snapshots   <- getSnapshotsMap
  pinnedId    <- either error' return $ resolveSnapshot snapshots spec
  configPath  <- ensureCachedConfig pinnedId
  generateProjectFile configPath

passthroughCmd :: String -> Maybe SnapshotSpec -> [String] -> IO ()
passthroughCmd cabalCmd mSpec extraArgs = do
  projectFile <- setupProject mSpec
  runCabal projectFile (cabalCmd : extraArgs)

refreshCmd :: Maybe SnapshotSpec -> IO ()
refreshCmd mSpec = do
  spec        <- effectiveSnapshot mSpec
  snapshots   <- getSnapshotsMap
  pinnedId    <- either error' return $ resolveSnapshot snapshots spec
  configPath  <- forceRefreshConfig pinnedId
  putStrLn $ "Refreshed: " ++ configPath

snapshotCmd :: Maybe String -> IO ()
snapshotCmd Nothing = do
  spec        <- effectiveSnapshot Nothing
  snapshots   <- getSnapshotsMap
  case resolveSnapshot snapshots spec of
    Right pinnedId -> putStrLn $ "Current snapshot: " ++ pinnedId
    Left _         -> putStrLn $ "Current snapshot: " ++ T.unpack (renderSnapshotSpec spec)
snapshotCmd (Just specStr) = do
  case parseSnapshotSpec specStr of
    Nothing   -> error' $ "Invalid snapshot spec: " ++ specStr
    Just spec -> do
      writeProjectSnapshot spec
      putStrLn $ "Snapshot set to: " ++ specStr ++ " (saved to .cabal-stackage)"

buildAllCmd :: [String] -> IO ()
buildAllCmd specStrs = do
  snapshots   <- getSnapshotsMap
  specs <- if null specStrs
    then return (defaultLtsSpecs snapshots)
    else mapM parseSpec specStrs
  forM_ specs $ \spec -> do
    let specStr = T.unpack (renderSnapshotSpec spec)
    putStrLn $ "\n=== " ++ specStr ++ " ==="
    case resolveSnapshot snapshots spec of
      Left err -> do
        error' err
      Right pinnedId -> do
        configPath  <- ensureCachedConfig pinnedId
        projectFile <- generateProjectFile configPath
        cmd_ "cabal" ["--project-file=" ++ projectFile, "build"]
  where
    parseSpec s =
      maybe (error' $ "Invalid snapshot spec: " ++ s) return (parseSnapshotSpec s)

-- | Nightly followed by LTS majors in descending order down to lts-16.
defaultLtsSpecs :: SnapshotsMap -> [SnapshotSpec]
defaultLtsSpecs snapshots =
  NightlyLatest :
  [ LtsMajor n
  | n <- [64,63..16]
  , Map.member (T.pack $ "lts-" ++ show n) snapshots
  ]
