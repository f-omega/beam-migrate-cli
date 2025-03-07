{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Database.Beam.Migrate.Cli.Engine.Database.Migrations where

import Database.Beam
import Database.Beam.Backend.SQL
import Database.Beam.Migrate
import Database.Beam.Migrate.Backend
import Database.Beam.Migrate.Cli.Engine.Database
--import Database.Beam.Migrate.Cli.Types
import Database.Beam.Schema.Tables
import Database.Beam.Query.Adhoc

import Control.Lens ((^.))

import Data.Int (Int32)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Time (LocalTime)

-- Internal migrations

beamMigrateDbCurrentVersion :: Int32
beamMigrateDbCurrentVersion = 1

getMigrationsFrom :: forall be m
                   . HasDefaultSqlDataType be LocalTime
                  => BeamMigrationBackend be m
                  -> DatabaseSettings be BeamMigrateDb
                  -> Int32 -> [(Int32, [BeamSqlBackendSyntax be])]
getMigrationsFrom (BeamMigrationBackend {}) migrateDb n = drop (fromIntegral n) migrations
  where
    migrations = zip [1..] [migration1]

    migration1 :: [ BeamSqlBackendSyntax be ]
    migration1 = [ createTableCmd $
                   createTableSyntax Nothing logTableName
                     [ ("id", columnSchemaSyntax (sqlDataType @Int32 False) Nothing [ notNull, primaryKey ] Nothing)
                     , ("name", columnSchemaSyntax (sqlDataType @Text False) Nothing [ notNull ] Nothing)
                     , ("branch", columnSchemaSyntax (sqlDataType @Text False) Nothing [] Nothing)
                     , ("action", columnSchemaSyntax (sqlDataType @Text False) Nothing [ notNull ] Nothing)
                     , ("user", columnSchemaSyntax (sqlDataType @Text False) Nothing [ notNull ] Nothing)
                     , ("date", columnSchemaSyntax (sqlDataType @LocalTime False) Nothing [ notNull ] Nothing)
                     , ("note", columnSchemaSyntax (sqlDataType @Text False) Nothing [ notNull ] Nothing)
                     , ("hash", columnSchemaSyntax (sqlDataType @Text False) Nothing [] Nothing)
                     , ("beam_migrate_version", columnSchemaSyntax (sqlDataType @Int32 False) Nothing [ notNull ] Nothing)
                     ]
                     [ ]
                 ]

    sqlDataType :: forall ty. HasDefaultSqlDataType be ty
                => Bool -> Sql92DdlCommandDataTypeSyntax (BeamSqlBackendSyntax be)
    sqlDataType = defaultSqlDataType (Proxy @ty) (Proxy @be)

    table1 = logTableVersion1 migrateDb
    logTableName = tableName (table1 ^. dbEntityDescriptor . dbEntitySchema)
                             (table1 ^. dbEntityDescriptor . dbEntityName)

    notNull = constraintDefinitionSyntax Nothing notNullConstraintSyntax Nothing
    primaryKey = constraintDefinitionSyntax Nothing primaryKeyColumnConstraintSyntax Nothing

--     getCommands :: Migration be () -> [BeamSqlBackendSyntax be]
--     getCommands m = migrateScript (\_ -> []) (\x -> [x]) (migrationStep mempty (\() -> m))
-- 
--     migrations :: [Migration be ()]
--     migrations =
--         [ migration1 ]
-- 
--     migration1 :: Migration be ()
--     migration1 = do
--       let table1 = logTableVersion1 migrateDb
--       createTable (table1 ^. checkedEntityDescriptor . unChecked . dbEntityName)
--                   (_ (table1 ^. checkedEntityDescriptor))
-- 
--       pure ()
-- 
