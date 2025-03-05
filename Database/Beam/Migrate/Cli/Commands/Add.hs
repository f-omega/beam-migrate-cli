{-# LANGUAGE OverloadedStrings #-}

module Database.Beam.Migrate.Cli.Commands.Add where

import           Database.Beam.Migrate.Cli.Commands.Common
import           Database.Beam.Migrate.Cli.Message
import           Database.Beam.Migrate.Cli.Registry
import           Database.Beam.Migrate.Cli.Types

import           Control.Lens ((^.), (&), (.~))
import           Control.Monad (unless, forM_)

import           Data.Maybe (maybeToList)
import qualified Data.Set as Set
import           Data.String (fromString)

import           System.Directory (createDirectory)

beamMigrateAdd :: BeamMigrateContext -> AddCmd -> IO ()
beamMigrateAdd ctxt add = do
  unless (isValidMigrationName (add ^. addMigrationName)) $ do
    beamMigrateError ctxt "Invalid migration name"

  branch <- getCurrentBranchOrDie ctxt
  mBranchStatus <- getLatestMigration ctxt branch

  mLastMigration <-
      case mBranchStatus of
        Just (_, BranchStatus Working _) -> beamMigrateError ctxt ("Could not add a new migration, because there is still a migration in progress: " <> pretty branch)
        Just (migrationName, _) -> pure (Just migrationName)
        _ -> pure Nothing

  forM_ (add ^. addMigrationDeps) $ \dep ->
    unless (isValidMigrationName dep) $
      beamMigrateError ctxt (pretty dep <> " is not a valid migration name for a dependency")

  commitMessage <- getOrEditMessage ctxt (commitMessageTemplate (add ^. addMigrationName)) (add ^. addCommitMessage)

  let deps = Set.fromList (maybeToList mLastMigration ++ add ^. addMigrationDeps)
      newMigration = defaultMigrationInfo
                     & miName .~ (add ^. addMigrationName)
                     & miCommitMessage .~ commitMessage
                     & miDependencies .~ (add ^. addMigrationDeps)
                     & miBranchStatus .~ [BranchStatus Working branch]

  modifyContextRegistry' ctxt $ \reg ->
    case addRegistryEntry reg (RegEntryMigration newMigration) of
      Left msg -> beamMigrateError ctxt ("Could not add registry entry: " <> fromString msg)
      Right reg -> pure reg

  -- Create structure for this migration
  ensureMigrationsDir ctxt

  schemaRecord <-
      case mLastMigration of
        Nothing -> pure emptyMigrationSchema
        Just migration -> do
          schema <- readMigrationSchema ctxt migration
          pure (emptyMigrationSchema & msStartingSchema .~ (schema ^. msEndingSchema)) 

  let newDirName = fullFilePath ctxt (MigrationDir (add ^. addMigrationName))
  createDirectory newDirName

  writeMigrationSchema ctxt (add ^. addMigrationName) schemaRecord
  notifyWriteFile ctxt (MigrateFile (add ^. addMigrationName) ApplyScript) applyScriptTemplate
  notifyWriteFile ctxt (MigrateFile (add ^. addMigrationName) RevertScript) revertScriptTemplate
  notifyWriteFile ctxt (MigrateFile (add ^. addMigrationName) VerifyScript) verifyScriptTemplate

  beamMigrateMessage ctxt ("Files were written to " <> filename newDirName)

applyScriptTemplate, revertScriptTemplate, verifyScriptTemplate :: String
applyScriptTemplate = "-- Insert SQL commands here to apply this migration"
revertScriptTemplate = "-- Insert SQL commands here to revert this migration"
verifyScriptTemplate = "-- Insert SQL commands here to verify this migration"
