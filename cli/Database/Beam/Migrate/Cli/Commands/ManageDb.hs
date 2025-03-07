{-# LANGUAGE OverloadedStrings #-}

module Database.Beam.Migrate.Cli.Commands.ManageDb where

import           Database.Beam
import           Database.Beam.Backend.SQL (insertCmd)
import           Database.Beam.Migrate (SomeDatabasePredicate(..), TableExistsPredicate (..), IsCheckedDatabaseEntity (unChecked), QualifiedName (..))
import           Database.Beam.Migrate.Backend (BeamMigrationBackend(..), DdlError)
import           Database.Beam.Migrate.Cli.Commands.Common
import           Database.Beam.Migrate.Cli.Engine.Internal
import           Database.Beam.Migrate.Cli.Message
import           Database.Beam.Migrate.Cli.Registry
import           Database.Beam.Migrate.Cli.Types hiding (Revert)
import           Database.Beam.Migrate.Simple (VerificationResult(VerificationSucceeded, VerificationFailed))
import           Database.Beam.Schema.Tables (dbEntityName, dbEntitySchema)

import           Control.Lens ((^.))
import           Control.Monad (when, join, forM_, unless)
import           Control.Monad.Trans (liftIO)

import           Data.Int (Int32)
import           Data.Maybe (isJust)
import           Data.String (fromString)
import qualified Data.Text as T
import           Data.Time (ZonedTime(zonedTimeToLocalTime), getZonedTime)


beamMigrateManageDb :: BeamMigrateContext -> ManageDb -> IO ()
beamMigrateManageDb ctx cmd =
  case cmd of
    InitDb cmd' -> beamMigrateInitDb ctx cmd'
    StatusDb cmd' -> showDatabaseStatus ctx (cmd' ^. statusDbForce)

beamMigrateInitDb :: BeamMigrateContext -> InitDbCmd -> IO ()
beamMigrateInitDb ctx cmd = do
  BeamDatabaseRunner { bdbrMigrateDb = migrateDb'
                     , bdbrBackend = be@BeamMigrationBackend { backendRenderSyntax = renderSyntax }
                     , bdbrRun = run } <- getRunner ctx
  let migrateDb = queryableMigrateDb migrateDb'

  migrateDbStatus <- checkMigrateDb ctx
  tableExists <- case migrateDbStatus of
                   Nothing ->
                       -- Very simple query just to sanity check
                      isJust <$> (hushDdlError ctx $ run $ runSelectReturningOne $ select $ limit_ 1 $ fmap (\l -> as_ @Int32 $ val_ (1 :: Int32)) $ all_ (migrateDb ^. bmdbLog))
                   Just VerificationSucceeded -> pure True
                   Just (VerificationFailed missing) -> pure (migrateTableExists ctx missing)

  mLastVersion <- if tableExists
                  then hushDdlError ctx (getLastBeamMigrateVersion ctx)
                  else pure Nothing

  today <- zonedTimeToLocalTime <$> getZonedTime

  let repair = beamMigrateRepairDb ctx cmd
      migrateFrom v =
          let migrations = getMigrationsFrom be migrateDb (fromIntegral v)
              runSql from to sql = do
                liftIO (beamMigrateMessage ctx $
                        "Migrate from " <> pretty from <> " to " <> pretty to <> if isVerbose ctx then ":" else "")

                forM_ sql $ \line -> do
                  when (isVerbose ctx || cmd ^. initDbDryRun) $ do
                    liftIO (beamMigrateMessage ctx $
                            indent 2 (fromString (renderSyntax line)))

                  unless (cmd ^. initDbDryRun) $ do
                    runNoReturn line

                when (isVerbose ctx || cmd ^. initDbDryRun) $
                  liftIO (beamMigrateMessage ctx (indent 2 "---"))

          in dieOnDdlError ctx $ run $ do
               forM_ (zip [v..] migrations) $ \(i, next) ->
                   let from = max 0 (i - 1)
                   in runSql from i next
               entryId <- getNextEntryId migrateDb
               let insertVersion = insert (migrateDb ^. bmdbLog)
                                   $ insertExpressions
                                   [ BeamMigrateLog { _bmlId = val_ entryId
                                                    , _bmlName = val_ (MigrationName "beam-migrate")
                                                    , _bmlBranch = nothing_
                                                    , _bmlAction = val_ System
                                                    , _bmlUser = val_ "" -- TODO
                                                    , _bmlDate = val_ today
                                                    , _bmlNote = val_ ""
                                                    , _bmlMigrateHash = nothing_
                                                    , _bmlMigrateVersion = val_ beamMigrateDbCurrentVersion } ]
                   insertVersionCmd = case insertVersion of
                                        SqlInsert _ i -> insertCmd i
                                        _ -> error "impossible"
               whenVerbose ctx $ liftIO (beamMigrateMessage ctx (fromString (renderSyntax insertVersionCmd)))
               runInsert insertVersion

  case mLastVersion of
    Nothing | tableExists -> repair
            | otherwise -> do
                beamMigrateInfo ctx "It doesn't look like this database has been touched before. Initializing new migrations tables"
                migrateFrom 0
    Just Nothing -> repair
    Just (Just version)
        | version < beamMigrateDbCurrentVersion -> do
            beamMigrateInfo ctx ("Running migration from version " <> pretty version)
            migrateFrom version
        | version == beamMigrateDbCurrentVersion -> do
            case migrateDbStatus of
              Just (VerificationFailed {}) -> repair
              _ -> pure ()
        | otherwise ->
            if cmd ^. initDbRepair && cmd ^. initDbForce
               then do
                 beamMigrateInfo ctx (warning "This database was touched by a newer version of beam-migrate")
                 repair
               else do
                 beamMigrateInfo ctx (warning ("This database was touched by a newer version of beam-migrate" <> line <>
                                               "Refusing to do anything. Rerun with '--repair --force' to force an automated repair"))

beamMigrateRepairDb :: BeamMigrateContext -> InitDbCmd -> IO ()
beamMigrateRepairDb ctx cmd = fail "repairdb"

migrateTableExists :: BeamMigrateContext -> [SomeDatabasePredicate] -> Bool
migrateTableExists ctx ps =
    case ctx ^. bmcRunner of
      Just BeamDatabaseRunner { bdbrMigrateDb = BeamMigrateDbCheckable b
                              , bdbrBackend = BeamMigrationBackend {} } ->
          not (SomeDatabasePredicate (TableExistsPredicate (QualifiedName (b ^. bmdbLog . checkedEntityDescriptor . unChecked . dbEntitySchema)
                                                                          (b ^. bmdbLog . checkedEntityDescriptor . unChecked . dbEntityName))) `elem` ps)

      Just _ -> True
      Nothing -> noDatabaseError ctx

getLastBeamMigrateVersion :: BeamMigrateContext -> IO (Either DdlError (Maybe Int32))
getLastBeamMigrateVersion ctx = do
  BeamDatabaseRunner { bdbrMigrateDb = migrateDb'
                     , bdbrBackend = BeamMigrationBackend {}
                     , bdbrRun = run } <- getRunner ctx
  let migrateDb = queryableMigrateDb migrateDb'
  run $ runSelectReturningOne $
    select $ fmap (\l -> l ^. bmlMigrateVersion) $
    orderBy_ (\l -> desc_ (l ^. bmlId)) $
    filter_ (\l -> l ^. bmlAction ==. val_ System) $
    all_ (_bmdbLog migrateDb) -- Not using orderedLogEntries on  purpose

showDatabaseStatus :: BeamMigrateContext -> Bool -> IO ()
showDatabaseStatus ctx forceDb = do
  mBranch <- getCurrentBranch ctx
  let statusLine = beamMigrateMessage ctx . info
  migrateDbStatus <- checkMigrateDb ctx
  shouldCheckDb <-
    case migrateDbStatus of
      Nothing -> statusLine (warning "This backend does not support checking the migrations table.") >> return True
      Just (VerificationFailed missing) -> do
        if migrateTableExists ctx missing
           then do
             statusLine ("The beam migrations table is " <> errorMessage "not up-to-date" <> " in this database.")

             -- Get the last version, if available
             mLastVersion <- hushDdlError ctx (getLastBeamMigrateVersion ctx)

             let corruptionError = statusLine (error "  The database is corrupt" <> line <>
                                               "  Run " <> cmdline ctx "db init --repair" <> " to attempt repair.")

             case join mLastVersion of
               Nothing -> corruptionError
               Just version
                    | version < beamMigrateDbCurrentVersion  -> statusLine ("  Run " <> cmdline ctx "db init" <> " to update the database (or run a command that affects the database)")
                    | version == beamMigrateDbCurrentVersion -> corruptionError
                    | otherwise -> statusLine (error "  It looks like this table was changed by a future version of beam-migrate.")

           else do
             statusLine ("The beam migrations table " <> errorMessage "does not exist" <> " in this database." <> line <>
                         "  Run " <> cmdline ctx "db init" <> " to initialize the database.")

        whenVerbose ctx $ do
          statusLine (indent 2 (reportMissingChecks ctx missing))

        return False
      Just VerificationSucceeded -> do
        -- Get the last version, if available
        mLastVersion <- hushDdlError ctx (getLastBeamMigrateVersion ctx)

        case mLastVersion of
          Nothing -> statusLine ("The beam migrations database seems up-to-date, but " <> errorMessage "cannot be accessed." <> line <>
                                 "  Run " <> cmdline ctx "db init --repair" <> " to attempt repair.")
          Just Nothing -> statusLine ("The beam migrations database is " <> success "up-to-date" <> ", but is " <> errorMessage "missing a version." <> line <>
                                      "  Run " <> cmdline ctx "db init --repair" <> " to attempt repair.")
          Just (Just version)
              | version < beamMigrateDbCurrentVersion ->
                  statusLine ("The beam migrations database is at version " <> info (pretty version) <> " and needs to " <> info "be upgraded." <> line <>
                              "  Run " <> cmdline ctx "db init" <> " to upgrade now")
              | version == beamMigrateDbCurrentVersion ->
                  statusLine ("The beam migrations database is " <> success "up-to-date")
              | otherwise -> statusLine (error "It looks like this database was changed by a future version of beam-migrate.")

        return True

  when (shouldCheckDb || forceDb) $ do
    mLogEntry <- getLatestLogEntry ctx

    case mLogEntry of
      Nothing -> statusLine (info "There have been no changes made to the database")
      Just logEntry -> do
        changeName <- case logEntry ^. bmlAction of
                        Apply -> pure (pretty ((logEntry ^. bmlName)))
                        Revert -> pure (info "a reversion (TODO)")
                        System -> error "impossible"

        let noteMessage | T.null (logEntry ^. bmlNote) = mempty
                        | otherwise = line <> line <> hang 2 (info "Change note:" <> line <> pretty (logEntry ^. bmlNote))

            branchMessage
              | mBranch /= logEntry ^. bmlBranch =
                  case logEntry ^. bmlBranch of
                    Just onBranch -> line <> "(which was committed on branch " <> pretty onBranch <> ")"
                    Nothing -> line <> "(which was not committed on " <> info "any branch" <> ")"
              | otherwise = mempty

            versionMessage = mempty

        statusLine (hang 2 ("The last change to the database was " <> changeName <> line <>
                            "which was committed to the database by " <> pretty (logEntry ^. bmlUser) <> line <>
                            "on " <> pretty (logEntry ^. bmlDate) <> line <>
                            branchMessage <>
                            versionMessage <>
                            noteMessage))
