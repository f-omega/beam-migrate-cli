{-# LANGUAGE OverloadedStrings #-}

module Database.Beam.Migrate.Cli.Commands.Abort where

import Database.Beam.Migrate.Cli.Commands.Common
import Database.Beam.Migrate.Cli.Message
import Database.Beam.Migrate.Cli.Registry
import Database.Beam.Migrate.Cli.Types

import Control.Lens ((&), (%~), (^.), view)
import Control.Monad (unless)

import Data.Maybe (fromMaybe)

beamMigrateAbort :: BeamMigrateContext -> AbortCmd -> IO ()
beamMigrateAbort ctxt abort = do
  curBranch <- getCurrentBranchOrDie ctxt
  let branch = fromMaybe curBranch (abort ^. abortBranchName)

  unless (branch == curBranch || abort ^. abortForce) $
    beamMigrateError ctxt ("Cannot abort commit on non-local branch (use --force if you really want to do this)")

  mBranchStatus <- getLatestMigration ctxt branch
  case mBranchStatus of
    Just (migrationName, BranchStatus Working fromBranch) -> do
      unless (branch == fromBranch || abort ^. abortForce) $
        beamMigrateError ctxt "It looks like this branch was forked from another branch with an uncommitted migration (use --force to abort migration on that branch)"

      -- Remove this branch the migration
      Just migration <- getMigration ctxt migrationName
      let migration' = migration & miBranchStatus %~ filter ((/= fromBranch) . view branchStatusBranch)

      if null (migration' ^. miBranchStatus)
         then do
           unless (abort ^. abortKeepFiles) $ do
             beamMigrateInfo ctxt ("Removing files for " <> pretty migrationName)
             deleteMigration ctxt migration'

           unless (abort ^. abortKeep) $ do
             beamMigrateInfo ctxt ("Migration " <> pretty migrationName <> " deregistered")
             unregisterMigration ctxt migrationName
         else do
           beamMigrateInfo ctxt ("Retaining migration " <> pretty migrationName <> " because it is in use by other branches")
           updateMigration ctxt migration'

      printStatusShort ctxt

    _ -> do beamMigrateInfo ctxt "No uncommitted migration present on this branch"

