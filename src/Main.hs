module Main (main) where

import           Control.Monad    (forM_, when)
import qualified Data.Map.Strict  as Map
import qualified Data.Text        as T
import           SimpleCmd        (cmd_, error', needProgram, warning)
import           SimpleCmdArgs
import           System.Directory (findExecutable, withCurrentDirectory)

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
              passthroughCmd "build" <$> debugOpt <*> snapshotOpt <*> many (strArg "ARGS")
          , Subcommand "install"   "Install the project" $
              passthroughCmd "install" <$> debugOpt <*> snapshotOpt <*> many (strArg "ARGS")
          , Subcommand "test"      "Test the project" $
              passthroughCmd "test" <$> debugOpt <*> snapshotOpt <*> many (strArg "ARGS")
          , Subcommand "run"       "Run an executable" $
              passthroughCmd "run" <$> debugOpt <*> snapshotOpt <*> many (strArg "ARGS")
          , Subcommand "repl"      "Open a REPL" $
              passthroughCmd "repl" <$> debugOpt <*> snapshotOpt <*> many (strArg "ARGS")
          , Subcommand "haddock"   "Build documentation" $
              passthroughCmd "haddock" <$> debugOpt <*> snapshotOpt <*> many (strArg "ARGS")
          , Subcommand "clean"     "Clean build artifacts" $
              passthroughCmd "clean" <$> debugOpt <*> snapshotOpt <*> many (strArg "ARGS")
          , Subcommand "refresh"   "Force re-download of snapshot config" $
              refreshCmd <$> snapshotOpt
          , Subcommand "snapshot"  "Show or set the current snapshot" $
              snapshotCmd <$> optional (strArg "SPEC")
          , Subcommand "build-all" "Build against multiple snapshots (stack-all equivalent)" $
              buildAllCmd <$> debugOpt <*> many (strArg "SPEC")
          ]
      where
        debugOpt :: Parser Bool
        debugOpt = switchLongWith "debug" "Show snapshot, compiler, and cabal command"

        snapshotOpt :: Parser (Maybe SnapshotSpec)
        snapshotOpt =
          optional $
            optionWith (maybeReader parseSnapshotSpec) 's' "snapshot" "SPEC"
              "Stackage snapshot (e.g. lts, lts-24, lts-24.31, nightly)"

-- | Resolve snapshot, ensure config is cached, generate project file.
-- Returns (projectFile, compilerArgs) where compilerArgs is ["-w", path]
-- if the snapshot's GHC is found on PATH, otherwise [].
setupProject :: Bool -> Maybe SnapshotSpec -> IO (FilePath, [String])
setupProject debug mSpec = do
  spec         <- effectiveSnapshot mSpec
  snapshots    <- getSnapshotsMap
  pinnedId     <- either error' return $ resolveSnapshot snapshots spec
  when debug $ warning $ "Snapshot: " ++ T.unpack (renderSnapshotSpec spec) ++ " -> " ++ pinnedId
  configPath   <- ensureCachedConfig pinnedId
  when debug $ warning $ "Config: " ++ configPath
  projectFile  <- generateProjectFile configPath
  when debug $ warning $ "Project file: " ++ projectFile
  compilerArgs <- resolveCompiler debug configPath
  return (projectFile, compilerArgs)

-- | Look up the compiler named in the cabal.config on PATH.
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
          return ["-w", path]
        Nothing   -> do
          warning $ compiler ++ " not found on PATH"
          return []

passthroughCmd :: String -> Bool -> Maybe SnapshotSpec -> [String] -> IO ()
passthroughCmd cabalCmd debug mSpec extraArgs = do
  (projectFile, compilerArgs) <- setupProject debug mSpec
  let allArgs = cabalCmd : compilerArgs ++ extraArgs
      projectArg = "--project-file=" ++ projectFile
  when debug $ warning $ "cabal " ++ unwords (projectArg : allArgs)
  runCabal projectFile allArgs

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
    Left _         -> putStrLn $ "Current snapshot: " ++ T.unpack (renderSnapshotSpec spec)
snapshotCmd (Just specStr) = do
  case parseSnapshotSpec specStr of
    Nothing   -> error' $ "Invalid snapshot spec: " ++ specStr
    Just spec -> do
      writeProjectSnapshot spec
      putStrLn $ "Snapshot set to: " ++ specStr ++ " (saved to .cabal-stackage)"

buildAllCmd :: Bool -> [String] -> IO ()
buildAllCmd debug specStrs = do
  snapshots <- getSnapshotsMap
  specs <- if null specStrs
    then return (defaultLtsSpecs snapshots)
    else mapM parseSpec specStrs
  forM_ specs $ \spec -> do
    let specStr = T.unpack (renderSnapshotSpec spec)
    putStrLn $ "\n=== " ++ specStr ++ " ==="
    case resolveSnapshot snapshots spec of
      Left err -> error' err
      Right pinnedId -> do
        configPath   <- ensureCachedConfig pinnedId
        projectFile  <- generateProjectFile configPath
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
  | n <- [64,63..16]
  , Map.member (T.pack $ "lts-" ++ show n) snapshots
  ]
