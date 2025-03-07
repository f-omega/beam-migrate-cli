{-# LANGUAGE OverloadedStrings #-}

module Database.Beam.Migrate.Cli.Commands.Commit where

import Database.Beam.Migrate.Backend
import Database.Beam.Migrate.Cli.Commands.Common
import Database.Beam.Migrate.Cli.Engine.Internal
import Database.Beam.Migrate.Cli.Message
import Database.Beam.Migrate.Cli.Registry
import Database.Beam.Migrate.Cli.Types

import Control.Lens ((^.), (&), (.~))
import Control.Monad (unless)
import Control.Monad.Trans (lift, liftIO, MonadIO)

beamMigrateCommit :: BeamMigrateContext -> CommitCmd -> IO ()
beamMigrateCommit ctx cmd = do
  reg <- readCurrentRegistry ctx

  branch <- getCurrentBranchOrDie ctx
  mMigration <- getLatestMigration ctx branch

  case mMigration of
    Nothing -> beamMigrateMessage ctx ("No migrations in branch. Use " <> cmdline ctx "add" <> " to create a new migration.")
    Just (nm, BranchStatus Committed _) ->
        beamMigrateMessage ctx ("Migration " <> pretty nm <> " already committed. Nothing to do...")
    Just (nm, BranchStatus Working _) -> do
       let notAppliedError :: MonadIO m => m a
           notAppliedError = liftIO $ beamMigrateError ctx ("Staged migration has not been applied, so we cannot commit. Use " <> cmdline ctx "migrate --staged" <> " to apply the staged migration to the database")
       log <- maybe notAppliedError pure =<< getLatestLogEntry ctx

       let hasBeenApplied = log ^. bmlName == nm && log ^. bmlAction == Apply
           checkHasBeenApplied = do
             hash <- calcMigrationHash <$> readMigration ctx ApplyScript nm
             unless hasBeenApplied notAppliedError

             unless (Just hash == log ^. bmlMigrateHash) $
               beamMigrateError ctx ("Staged migration has changed since application, so we cannot commit. Use " <> cmdline ctx "migrate --staged" <> " to apply the staged migration to the database")
             pure ()

       unless (cmd ^. commitForce) checkHasBeenApplied

       migInfo <- maybe (beamMigrateError ctx ("Could not find migration " <> pretty nm <> " in registry"))
                        pure =<<
                  getMigration ctx nm

       let migInfo' = updateOrCreateBranchStatus migInfo (BranchStatus Committed branch)
       updateMigration ctx migInfo'

       -- Now save the migrations file
       sch <- readMigrationSchema ctx nm
       preds <- readCurrentDbPoint ctx
       let sch' = sch & msEndingSchema .~ preds
       writeMigrationSchema ctx nm sch'
       beamMigrateMessage ctx ("Migration " <> pretty nm <> " committed to branch " <> pretty branch)
