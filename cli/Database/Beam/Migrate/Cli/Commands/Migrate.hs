{-# LANGUAGE OverloadedStrings #-}

module Database.Beam.Migrate.Cli.Commands.Migrate where

import Database.Beam
import Database.Beam.Migrate.Backend (BeamMigrationBackend(..))
import Database.Beam.Migrate.Cli.Commands.Common
import Database.Beam.Migrate.Cli.Message
import Database.Beam.Migrate.Cli.Registry
import Database.Beam.Migrate.Cli.Types
import Database.Beam.Migrate.Cli.Database
import Database.Beam.Query.CustomSQL

import Control.Exception (SomeException(..), Exception, handle, throwIO, catch)
import Control.Lens ((^.), view)
import Control.Monad (unless)
import Control.Monad.Trans (lift)

import Data.Time
import Data.Maybe (maybeToList)
import Data.String (fromString)
import Database.Beam.Migrate.Cli.Database.Migrations (beamMigrateDbCurrentVersion)


data DontCommit = DontCommit deriving Show
instance Exception DontCommit

beamMigrateMigrate :: BeamMigrateContext -> MigrateCmd -> IO ()
beamMigrateMigrate ctx cmd = do
  migrationNames <- collectAppliedMigrations ctx
  reg <- readCurrentRegistry ctx

  -- Now find all migrations not dominated by
  let tips = migrationTips reg migrationNames
      dom = dominatorForTips reg tips
--  dom <- getDominator ctx tips
  -- We would have to undo all changes since the common dominator,
  -- then apply the merge change, if it exis
--  let undoChanges = map (reverse . findPath reg dom) tips
  beamMigrateMessage ctx ("dom is " <> pretty dom)

  branch <- getCurrentBranchOrDie ctx

  -- Now find the migration for this branch
  migration <- if cmd ^. migrateStaged
               then getLatestMigration ctx branch
               else getLatestCommittedMigration ctx branch

  beamMigrateMessage ctx ("migration is " <> pretty (fst <$> migration))

  today <- zonedTimeToLocalTime <$> getZonedTime
  user <- getCurrentUser ctx

  -- If dom is not the migration names bail
  unless (migrationNames == maybeToList dom) $
    beamMigrateError ctx ("Can't undo migrations yet")
  putStrLn ("Graph is " ++ show (regDomGraph reg))
  if dom == (fst <$> migration)
     then beamMigrateMessage ctx "Nothing to do"
     else do
       let toApply = migrationsFor reg dom (fst <$> migration)
       case toApply of
         Nothing -> beamMigrateError ctx "There is no way to migrate the database. Please add a new migration"
         Just path ->
           handle (\DontCommit -> pure ()) $
           dieOnDdlError ctx $
           runMigration ctx $ \BeamMigrationBackend { backendRunSqlScript = runScript
                                                    , backendWithTransaction = withTransaction } migrateDb -> do
             liftIO (beamMigrateMessage ctx ("migrations are " <> pretty (map (view miName) <$> toApply)))

             let undo partial applied = error "undo: TODO"

                 withAllMigrations | cmd ^. migrateTest = \go -> liftWithLower (\lower -> withTransaction (lower go)) >> liftIO (throwIO DontCommit)
                                   | otherwise = id
                 individualMigration | cmd ^. migrateTest = id
                                     | otherwise = \go -> liftWithLower (\lower -> withTransaction (lower go))

                 reportError ::  Show e => MigrationName -> e -> IO ()
                 reportError onMigration e = beamMigrateMessage ctx (errorMessage "Could not apply " <> pretty onMigration <> ": " <> pretty (show e))

                 go applied [] = pure True
                 go applied (next:nexts) = do
--                   handle (\(SomeException e) -> reportError (next ^. miName) e >> undo next applied) $ do
                     applyScript <- liftIO (readMigration ctx ApplyScript (next ^. miName))
                     liftIO (beamMigrateMessage ctx ("Run script " <> line <> text applyScript))
                     individualMigration $ do
                       lift (runScript applyScript)
                       nextId <- getNextEntryId migrateDb
                       runInsert $ insert (_bmdbLog migrateDb)
                                 $ insertExpressions [ BeamMigrateLog { _bmlId = val_ nextId
                                                                      , _bmlName = val_ (next ^. miName)
                                                                      , _bmlBranch = just_ (val_ branch)
                                                                      , _bmlAction = val_ Apply
                                                                      , _bmlUser = val_ user
                                                                      , _bmlDate = val_ today
                                                                      , _bmlNote = val_ (cmd ^. migrateNote)
                                                                      , _bmlMigrateHash = just_ $ val_ (calcMigrationHash applyScript)
                                                                      , _bmlMigrateVersion = val_ beamMigrateDbCurrentVersion
                                                                      }
                                                     ]
                     go (next:applied) nexts
             _success <- withAllMigrations (go [] path)
             pure ()

--             runMigration ctx $ \BeamMigrationBackend {} ->
--               let apply applied [] = pure (Right ())
--                   apply applied (next:nexts) = do
--                     runAndApplyMigration ctx next
