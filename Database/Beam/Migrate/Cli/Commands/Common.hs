{-# LANGUAGE OverloadedStrings #-}

module Database.Beam.Migrate.Cli.Commands.Common where

import           Database.Beam.Migrate (SomeDatabasePredicate(..), serializePredicate)
import           Database.Beam.Migrate.Cli.Registry
import           Database.Beam.Migrate.Cli.Types

import           Control.Exception (Exception, throwIO)
import           Control.Lens ((^.))

import qualified Data.Aeson as JSON
import qualified Data.Aeson.KeyMap as JSON
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import           System.FilePath ((</>))
import           System.IO (hPutStrLn, stderr)

import           Text.Editor (runUserEditorDWIM, plainTemplate)
import           System.Directory (createDirectoryIfMissing)

newtype BeamMigrateError = BeamMigrateError String
    deriving (Show, Exception)

beamMigrateError :: BeamMigrateContext -> String -> IO a
beamMigrateError ctxt s = do
  hPutStrLn stderr (ctxt ^. bmcOpts . beamMigrateProgName <> ": " <> s)
  throwIO (BeamMigrateError s)

beamMigrateMessage :: BeamMigrateContext -> String -> IO ()
beamMigrateMessage _ = putStrLn

beamMigrateInfo :: BeamMigrateContext -> String -> IO ()
beamMigrateInfo ctxt s =
    case ctxt ^. bmcVerbosity of
      Silent -> pure ()
      _ -> beamMigrateMessage ctxt s

getCurrentBranchOrDie :: BeamMigrateContext -> IO BranchName
getCurrentBranchOrDie ctxt = do
  mBranch <- (ctxt ^. bmcOpts . beamMigrateVcGetBranch) ctxt

  case mBranch of
    Nothing -> beamMigrateError ctxt "Could not determine the name of the VC branch. Are you in a version-controlled directory?"
    Just branch -> pure branch

getLatestMigration :: BeamMigrateContext -> BranchName -> IO (Maybe (MigrationName, BranchStatus))
getLatestMigration ctxt branchName = do
  registry <- readCurrentRegistry ctxt
  case findMigrationsForBranch registry branchName of
    [] -> pure Nothing
    [(mi, sts)] -> pure (Just (mi ^. miName, sts))
    xs -> needsMergeError ctxt branchName xs

needsMergeError :: BeamMigrateContext -> BranchName -> [(MigrationInfo, BranchStatus)] -> IO a
needsMergeError ctxt nm tips =
    beamMigrateError ctxt ("Branch " <> show nm <> " needs merge.\n" <>
                           show (length tips) <> " migrations are currently recorded as its tip.\n" <>
                           foldMap reportTip tips)
  where
    reportTip (mi, BranchStatus sts _) =
        "  - " <> case sts of { Working -> "* "; _ -> "" } <> show (mi ^. miName) <> "\n"

getOrEditMessage :: BeamMigrateContext -> Template -> Maybe Text -> IO Text
getOrEditMessage _ _ (Just x) = pure x
getOrEditMessage _ (Template x) Nothing = stripMessage . TE.decodeUtf8 <$> runUserEditorDWIM plainTemplate x

stripMessage :: Text -> Text
stripMessage = T.unlines . dropEmpty . filter (not . T.isPrefixOf (T.pack "#")) . T.lines
  where
    dropEmpty = reverse . dropWhile T.null . reverse . dropWhile T.null

-- * Files

data MigrateFile
    = ApplyScript
    | RevertScript
    | VerifyScript
    | SchemaFile
      deriving (Show, Eq, Ord)

migrateFileName :: MigrateFile -> FilePath
migrateFileName ApplyScript = "apply.sql"
migrateFileName RevertScript = "revert.sql"
migrateFileName VerifyScript = "verify.sql"
migrateFileName SchemaFile = "beam_schema"

data CanonicalFile
    = MigrateFile !MigrationName !MigrateFile
    | MigrationDir !MigrationName
      deriving (Show, Eq, Ord)

canonicalFileName :: CanonicalFile -> FilePath
canonicalFileName (MigrateFile mig file) =
  canonicalFileName (MigrationDir mig) </> migrateFileName file
canonicalFileName (MigrationDir (MigrationName mig)) =
  T.unpack mig

fullFilePath :: BeamMigrateContext -> CanonicalFile -> FilePath
fullFilePath ctxt fl@(MigrateFile {}) =
    ctxt ^. bmcMigrationsDir </> canonicalFileName fl

notifyWriteFile :: BeamMigrateContext -> CanonicalFile -> String -> IO ()
notifyWriteFile ctxt fl contents = do
  let fp = fullFilePath ctxt fl
  beamMigrateInfo ctxt ("Write file " <> fp <> "...")
  writeFile fp contents

commitMessageTemplate :: MigrationName -> Template
commitMessageTemplate (MigrationName nm) =
    mkTemplate
    [ "Commit message for '" <> T.unpack nm <> "'"
    , ""
    , "# Please type your commit message for this migration above."
    , "# A commit message can be multiple lines. The first line should"
    , "# be a concise descriptnio of the change."
    , ""
    , "# Some notes:"
    , "#"
    , "#   * Lines beginning with a '#' are treated as comments and ignored"
    , "#   * Empty lines at the beginning and end of the commit message"
    , "#     will be ignored"
    ]

ensureMigrationsDir :: BeamMigrateContext -> IO ()
ensureMigrationsDir bmc = createDirectoryIfMissing True (bmc ^. bmcMigrationsDir)

type SerializedMigrationSchema = JSON.KeyMap [JSON.Value]

readMigrationSchema :: BeamMigrateContext -> MigrationName -> IO MigrationSchema
readMigrationSchema ctxt nm = do
  schema :: SerializedMigrationSchema <-
    maybe (beamMigrateError ctxt ("Could not parse migration schema for " <> show nm)) pure =<<
    JSON.decodeFileStrict (fullFilePath ctxt (MigrateFile nm SchemaFile))

  starting <- case JSON.lookup "starting_schema" schema of
                Nothing -> beamMigrateError ctxt ("Could not read migration schema for " <> show nm <> ": starting_schema key not found")
                Just sch -> parseDatabasePoint ctxt sch

  ending <- case JSON.lookup "ending_schema" schema of
              Nothing -> beamMigrateError ctxt ("Could not read migration schema for " <> show nm <> ": ending_schema key not found")
              Just sch -> parseDatabasePoint ctxt sch

  pure MigrationSchema { _msStartingSchema = starting
                       , _msEndingSchema = ending }

parseDatabasePoint :: BeamMigrateContext -> [JSON.Value] -> IO DatabasePoint
parseDatabasePoint bmc _ = fail "parseDatabasePoint"

writeMigrationSchema :: BeamMigrateContext -> MigrationName -> MigrationSchema -> IO ()
writeMigrationSchema ctxt nm ms = do
  let starting = serializeDatabasePoint ctxt (ms ^. msStartingSchema)
      ending = serializeDatabasePoint ctxt (ms ^. msEndingSchema)

  JSON.encodeFile (fullFilePath ctxt (MigrateFile nm SchemaFile)) $
      JSON.object [ "starting_schema" JSON..= starting
                  , "ending_schema"   JSON..= ending ]

serializeDatabasePoint :: BeamMigrateContext -> DatabasePoint -> [JSON.Value]
serializeDatabasePoint ctxt (DatabasePoint cs) = map (\(SomeDatabasePredicate p) -> serializePredicate p) cs
