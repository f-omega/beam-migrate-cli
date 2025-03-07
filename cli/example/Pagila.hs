module Main where

import Database.Beam.Migrate.Cli
import qualified Database.Beam.Sqlite.Migrate as Sqlite

import Control.Lens ((&))
main = beamMigrateCli (defBeamMigrateOptions & registerBackend Sqlite.migrationBackend emptyDb)
