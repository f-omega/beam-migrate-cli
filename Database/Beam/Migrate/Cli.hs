{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Database.Beam.Migrate.Cli
     ( beamMigrateCli
     , BeamMigrateOptions(..)
     , registerBackend
     , defBeamMigrateOptions
     , emptyDb, EmptyDb
     , defaultGetCurrentUser ) where

import           Control.Exception (Exception, throwIO, catches, Handler(..), bracket, mask)
import           Control.Lens (makeLenses, (^.), (%~))
import           Control.Monad.Trans (MonadIO)

import           Database.Beam (MonadBeam, DatabaseSettings, HasSqlEqualityCheck)
import           Database.Beam.Migrate (CheckedDatabaseSettings, HasDefaultSqlDataType)
import           Database.Beam.Migrate.Backend
import           Database.Beam.Migrate.Cli.Commands.Common
import           Database.Beam.Migrate.Cli.Message (BeamMigrateError(..), errorMessage)
import           Database.Beam.Migrate.Cli.Registry
import           Database.Beam.Migrate.Cli.Types
import           Database.Beam.Migrate.Cli.VC
import           Database.Beam.Migrate.Simple (haskellSchema)

import           Database.Beam.Migrate.Cli.Commands.Abort (beamMigrateAbort)
import           Database.Beam.Migrate.Cli.Commands.Add (beamMigrateAdd)
import           Database.Beam.Migrate.Cli.Commands.Commit (beamMigrateCommit)
import           Database.Beam.Migrate.Cli.Commands.Diff (beamMigrateDiff)
import           Database.Beam.Migrate.Cli.Commands.Edit (beamMigrateEdit)
import           Database.Beam.Migrate.Cli.Commands.Log (beamMigrateLog)
import           Database.Beam.Migrate.Cli.Commands.ManageDb (beamMigrateManageDb)
import           Database.Beam.Migrate.Cli.Commands.Migrate (beamMigrateMigrate)
import           Database.Beam.Migrate.Cli.Commands.Pickle (beamMigratePickle)
import           Database.Beam.Migrate.Cli.Commands.Status (beamMigrateStatus)
import           Database.Beam.Migrate.Cli.Commands.Verify (beamMigrateVerify)
import           Database.Beam.Migrate.Cli.Database (BeamMigrateDb, BeamMigrateCliBackend, beamMigrateDb)

import           Control.Monad (when)
import           Data.IORef (newIORef, readIORef)
import           Data.List (find, isPrefixOf)
import           Data.String (fromString)
import qualified Data.Text as T
import           Data.Time (LocalTime)

import           Options.Applicative

import           System.Directory (doesFileExist)
import           System.FilePath (takeDirectory)
import           System.Environment (getProgName)


#ifdef mingw32_HOST_OS
import           System.Win32.Info (getUserName)
#else
import           System.Posix.User (getEffectiveUserID, getUserEntryForID, userName)
#endif

defBeamMigrateOptions :: BeamMigrateOptions
defBeamMigrateOptions = BeamMigrateOptions
                      { _beamMigrateCliDesc = "beam-migrate - Manage beam migrations"
                      , _beamMigrateProgName = "beam-migrate"
                      , _beamMigrateBackends = []
                      , _beamMigrateRunInDatabase = \_ -> pure Nothing
                      , _beamMigrateListDatabases = Nothing
                      , _beamMigrateAllowDirect = True
                      , _beamMigrateFindRegistry = defaultFindRegistry
                      , _beamMigrateFindMigrationsDir = defaultFindMigrationsDir
                      , _beamMigrateVcGetBranch = defaultVcGetBranch
                      , _beamMigrateGetCurrentUser = defaultGetCurrentUser
                      }

defaultGetCurrentUser :: IO T.Text
defaultGetCurrentUser = do
#ifdef mingw32_HOST_OS
  username <- getUserName
  pure (T.pack username)
#else
  uid <- getEffectiveUserID
  userEntry <- getUserEntryForID uid  -- Get user details
  pure (T.pack (userName userEntry))
#endif

cliOptsParser :: BeamMigrateOptions -> Parser BeamMigrateCliOptions
cliOptsParser bm =
    BeamMigrateCliOptions
      <$> switch (long "show-commands" <> help "Show commands as they're run")
      <*> verbosity
      <*> database
      <*> cliCommand

    where
      verbosity = silent <|> verbose <|> pure Normal
      silent = Silent <$ flag' () (long "quiet" <> short 'q' <> help "Only output necessary information")
      verbose = Verbose <$ flag' () (long "verbose" <> short 'v' <> help "Output copious information")

      database = connStr <|>
                 (NamedDb <$> strOption (long "database" <> short 'D' <> help "Name of database to connect to" <> metavar "NAME")) <|>
                 pure NoDb

      connStr
          | bm ^. beamMigrateAllowDirect =
              ConnStr <$> strOption (long "connstr" <> short 'C' <> help "Database connection string to use" <> metavar "CONNSTR")
          | otherwise = empty

      cliCommand = subparser . mconcat $
                   [ command "status" (info (statusCmd <**> helper) (progDesc "Get status of migration registry"))
                   , command "log" (info (logCmd <**> helper) (progDesc "Show changes made to database"))
                   , command "add" (info (addCmd <**> helper) (progDesc "Add a new migration to the registry"))
                   , command "abort" (info (abortCmd <**> helper) (progDesc "Abort a migration in progress"))
                   , command "migrate" (info (migrateCmd <**> helper) (progDesc "Get status of migration registry"))
                   , command "verify" (info (verifyCmd <**> helper) (progDesc "Get status of migration registry"))
                   , command "db" (info (dbCmd <**> helper) (progDesc "Manage the database directly"))

                   , command "edit" (info (editCmd <**> helper) (progDesc "Edit files for migration"))
                   , command "commit" (info (commitCmd <**> helper) (progDesc "Commit a migration to the registry"))
                   , command "diff" (info (diffCmd <**> helper) (progDesc "Get difference between two database specs"))
                   , command "pickle" (info (pickleCmd <**> helper) (progDesc "Generate a Haskell module containing a snapshot of the registry and migrations database"))
                   ]

      statusCmd = Status <$>
                  (StatusCmd <$> switch (help "Produce machine-readable output" <> long "porcelain" <> long "machine-readable")
                             <*> switch (help "Force a status check on the database even if the database is not configured correctly" <> long "force-db-status")
                  )

      logCmd = Log <$> (LogCmd <$> switch (help "Display short messages" <> long "short"))
      abortCmd = Abort <$> (AbortCmd
                            <$> optional (BranchName . T.pack <$> strOption (metavar "BRANCH" <>
                                                                             help "Name of branch to abort changes on" <>
                                                                             long "branch" <> short 'B'))
                            <*> switch (help "Force abort even if checks fail" <> short 'F' <> long "force")
                            <*> switch (help "Keep aborted migration record in registry, even if no other branch uses it" <>
                                        long "keep")
                            <*> switch (help "Keep aborted migration files around after abort" <>
                                        long "no-unlink")
                           )

      addCmd = Add <$> (AddCmd
                        <$> (MigrationName . T.pack <$> strArgument (metavar "NAME" <> help "Name of new migration"))
                        <*> many (MigrationName . T.pack <$> strArgument (metavar "DEP" <> help "Names of migrations this depends on. If not given, it will be inferred"))
                        <*> optional (T.pack <$> strOption (metavar "MESSAGE" <> help "Commit message to save with the migration" <> long "message" <> short 'm'))
                        )
      migrateCmd = Migrate <$> (MigrateCmd
                                <$> switch (help "Apply staged migrations" <> long "staged")
                                <*> switch (help "Do not run SQL commands, just display them" <> long "dry-run")
                                <*> switch (help "Do not commit changes, just try to apply them" <> long "test")
                                <*> (T.pack <$> strOption (long "note" <> short 'N' <> help "Note to add to log" <> value "")))
      verifyCmd  = Verify <$> (VerifyCmd <$> switch (help "Do not perform an internal sanity check. Just run the verify script" <> long "--skip-internal")
                                         <*> (fromString <$> strArgument (metavar "SOURCE" <> help "Source to read database from" <> value "HEAD")))

      dbCmd = ManageDb <$> subparser (mconcat dbCommands)
      dbCommands = [ command "init" (info (initDbCmd <**> helper) (progDesc "Initialize the database"))
                   , command "status" (info (statusDbCmd <**> helper) (progDesc "Report on the status of the database")) ]

      initDbCmd = InitDb <$> (InitDbCmd
                             <$> switch (long "dry-run" <> help "Do not make changes, just print out what you would do")
                             <*> switch (long "force" <> short 'F' <> help "Force initialization even if checks fail")
                             <*> switch (long "repair" <> help "Attempt to repair the table"))

      statusDbCmd = StatusDb <$> (StatusDbCmd
                                  <$> switch (long "force" <> short 'F' <> help "Force status check even if migration table checks fail"))

      editCmd = Edit <$> (EditCmd
                          <$> (MigrationName . T.pack <$> strArgument (metavar "NAME" <> help "The migration to edit"))
                          <*> (flag' ApplyScript (long "apply" <> help "Edit the apply script (default)") <|>
                               flag' RevertScript (long "revert" <> help "Edit the revert script") <|>
                               flag' VerifyScript (long "verify" <> help "Edit the verification script") <|>
                               pure ApplyScript))

      commitCmd = Commit <$> (CommitCmd
                              <$> switch (long "force" <> short 'F' <> help "Force commit even if the staged migration was not applied"))

      diffCmd = Diff <$> (DiffCmd
                          <$> (flag' HaskellFormat (long "haskell" <> long "hs" <> help "Output migration in haskell") <|>
                               flag' SourceFormat (long "source-format" <> help "Output migration in source syntax") <|>
                               flag' DestinationFormat (long "destination-format" <> help "Output migration in the destination source syntax") <|>
                               pure SomeFormat)
                          <*> (fromString <$> strArgument (metavar "SOURCE" <> help "Source database"))
                          <*> (fromString <$> strArgument (metavar "DEST" <> help "Destination database")))

      pickleCmd = Pickle <$> (PickleCmd
                              <$> (fromString <$> strOption (long "module" <> short 'M' <> value "Migrations" <> metavar "MODULE" <> help "Name of the haskell module to generate"))
                              <*> (SelectNamedBranches <$> some (BranchName . fromString <$> strArgument (metavar "BRANCH" <> help "Name of branches to include in the pickle")) <|>
                                   flag' SelectAllBranches (long "all" <> short 'A' <> help "Include migrations in all branches") <|>
                                   pure SelectDefaultBranch)
                              <*> many (MigrationName . fromString <$> strOption (metavar "MIGRATION" <> long "include" <> short 'I' <> help "Extra migrations (and dependencies) to include"))
                              <*> flag True False (long "dont-obfuscate" <> help "If given, do not obfuscate the migration names in the pickle")
                              <*> (SelectNamedBackends <$> some (fromString <$> strOption (metavar "BACKEND" <> long "backend" <> short 'B' <> help "Only include migrations for the given backends")) <|>
                                   pure SelectAllBackends)
                              <*> (flag' IncludeWorking (long "lax" <> help "Include uncommitted migrations") <|>
                                   flag' CommittedOnly (long "strict" <> help "Enforce that each branch not be at an uncommitted migration") <|>
                                   pure LastCommit))

beamMigrateCli :: BeamMigrateOptions -> IO ()
beamMigrateCli opts = do
  parsedOpts <- execParser (info (cliOptsParser opts <**> helper)
                            (progDesc (opts ^. beamMigrateCliDesc)))

  withBeamMigrateContext opts parsedOpts $ \ctxt -> do
    flip catches (excHandlers ctxt) $
      case cliCommand parsedOpts of
        Add cmd -> beamMigrateAdd ctxt cmd
        Abort cmd -> beamMigrateAbort ctxt cmd
        Log cmd -> beamMigrateLog ctxt cmd
        Status cmd -> beamMigrateStatus ctxt cmd
        ManageDb cmd -> beamMigrateManageDb ctxt cmd
        Migrate cmd -> beamMigrateMigrate ctxt cmd
        Edit cmd -> beamMigrateEdit ctxt cmd
        Commit cmd -> beamMigrateCommit ctxt cmd
        Verify cmd -> beamMigrateVerify ctxt cmd
        Diff cmd -> beamMigrateDiff ctxt cmd
        Pickle cmd -> beamMigratePickle ctxt cmd
        _ -> putStrLn "Not implemented"
  where
    excHandlers ctx = [ Handler (\(BeamMigrateError e) -> beamMigrateMessage ctx (errorMessage "Error: " <> e)) ]

registerBackendWithCustomBeamMigrateDbUnchecked
    :: (HasSqlEqualityCheck be T.Text, MonadIO m, HasDefaultSqlDataType be LocalTime)
    => BeamMigrationBackend be m -> CheckedDatabaseSettings be db
    -> DatabaseSettings be BeamMigrateDb
    -> BeamMigrateOptions -> BeamMigrateOptions
registerBackendWithCustomBeamMigrateDbUnchecked be db beamMigrateDb =
    beamMigrateBackends %~ (SomeCliMigrationBackend be db (BeamMigrateDbUncheckable beamMigrateDb):)

registerBackendWithCustomBeamMigrateDb
    :: (HasSqlEqualityCheck be T.Text, MonadIO m, HasDefaultSqlDataType be LocalTime)
    => BeamMigrationBackend be m -> CheckedDatabaseSettings be db
    -> CheckedDatabaseSettings be BeamMigrateDb
    -> BeamMigrateOptions -> BeamMigrateOptions
registerBackendWithCustomBeamMigrateDb be db beamMigrateDb =
    beamMigrateBackends %~ (SomeCliMigrationBackend be db (BeamMigrateDbCheckable beamMigrateDb):)

registerBackend :: ( HasSqlEqualityCheck be T.Text
                   , BeamMigrateCliBackend be
                   , HasDefaultSqlDataType be LocalTime
                   , MonadIO m )
                => BeamMigrationBackend be m -> CheckedDatabaseSettings be db
                -> BeamMigrateOptions -> BeamMigrateOptions
registerBackend be db = registerBackendWithCustomBeamMigrateDb be db beamMigrateDb

withBeamMigrateContext :: BeamMigrateOptions -> BeamMigrateCliOptions
                       -> (BeamMigrateContext -> IO a) -> IO a
withBeamMigrateContext opts cli go =
    mask $ \unmask -> do
      ctx <- openBeamMigrateContext opts cli
      b <- unmask (go ctx)
      finalizeBeamMigrateContext ctx
      pure b

openBeamMigrateContext :: BeamMigrateOptions -> BeamMigrateCliOptions -> IO BeamMigrateContext
openBeamMigrateContext opts cli = do
  mRunner <- case cliDatabase cli of
               NamedDb nm -> Just <$> (opts ^. beamMigrateRunInDatabase) nm
               ConnStr conn ->
                   case find (isBackendConnStr conn) (opts ^. beamMigrateBackends) of
                     Nothing -> pure Nothing
                     Just (SomeCliMigrationBackend be@BeamMigrationBackend { backendConnect = connect } db beamMigrateDb) -> do
                         let conn' = drop (length (backendName be) + 1) conn
                         BeamMigrateConnection { backendRun = run, backendClose = close } <- connect conn'
                         pure (Just (Just (BeamDatabaseRunner be db beamMigrateDb run close)))
               NoDb -> pure Nothing

  runner <- case mRunner of
              Nothing -> pure Nothing
              Just Nothing -> fail "Could not open database"
              Just runner -> pure runner

  -- Read registry
  registryFile <- opts ^. beamMigrateFindRegistry
  registryExists <- doesFileExist registryFile
  registry <- if registryExists then readRegistry registryFile else pure newRegistry

  migrationsDir <- (opts ^. beamMigrateFindMigrationsDir) registryFile

  registryVar <- newIORef (newRegistryStatus registry)

  exeName <- getProgName

  pure BeamMigrateContext { _bmcRunner = runner
                          , _bmcDatabase = cliDatabase cli
                          , _bmcOpts = opts
                          , _bmcRegistryRef = registryVar
                          , _bmcRegistryFile = registryFile
                          , _bmcTopDir = takeDirectory registryFile
                          , _bmcMigrationsDir = migrationsDir
                          , _bmcVerbosity = cliVerbosity cli
                          , _bmcExeName = exeName
                          , _bmcDumpSql = cliVerbosity cli == Verbose }
  where
    isBackendConnStr :: String -> SomeCliMigrationBackend -> Bool
    isBackendConnStr conn (SomeCliMigrationBackend BeamMigrationBackend { backendName = nm } _ _) = (nm ++ ":") `isPrefixOf` conn

finalizeBeamMigrateContext :: BeamMigrateContext -> IO ()
finalizeBeamMigrateContext ctxt = do
  regsts <- readIORef (ctxt ^. bmcRegistryRef)
  when (registryNeedsWrite regsts) $ do
    writeRegistry (ctxt ^. bmcRegistryFile) (getCurrentRegistry regsts)
