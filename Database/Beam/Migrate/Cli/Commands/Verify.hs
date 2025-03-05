module Database.Beam.Migrate.Cli.Commands.Verify where

import Database.Beam.Migrate.Backend
import Database.Beam.Migrate.Cli.Commands.Common
import Database.Beam.Migrate.Cli.Message
import Database.Beam.Migrate.Cli.Registry
import Database.Beam.Migrate.Cli.Types

import Control.Lens ((^.))
import Control.Monad (when, unless)
import Control.Monad.Trans (lift)


beamMigrateVerify :: BeamMigrateContext -> VerifyCmd -> IO ()
beamMigrateVerify ctx cmd =
  withDbSource ctx (cmd ^. verifySource) $ \src -> do
    unless (cmd ^. verifyIgnoreInternal) $ do
      predicates <- readDatabasePointFromSource ctx src
      expected <- readCurrentDbPoint ctx

      let diff = diffPoints predicates expected

      unless (noDifference diff) $
        reportDatabaseDifference ctx diff

    mMigration <- getMigrationFromDbSource ctx src
    case mMigration of
      Nothing -> pure ()
      Just nm -> do
        script <- readMigration ctx VerifyScript nm
        dieOnDdlError ctx $ runMigration ctx $ \BeamMigrationBackend { backendRunSqlScript = runScript } _ ->
          lift (runScript script)
