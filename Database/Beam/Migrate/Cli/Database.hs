{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}

module Database.Beam.Migrate.Cli.Database where

import           Database.Beam
import           Database.Beam.Backend (BeamBackend, BeamSqlBackend, BeamSqlBackendSupportsDataType, SqlNull)
import           Database.Beam.Backend.SQL (HasSqlValueSyntax (..))
import           Database.Beam.Migrate.Cli.Registry (MigrationName, BranchName)
import           Database.Beam.Schema.Tables (dbEntityName)
import           Database.Beam.Migrate ( CheckedDatabaseSettings, defaultMigratableDbSettings
                                       , renameCheckedEntity, modifyCheckedTable, checkedTableModification
                                       , checkedFieldNamed, BeamMigrateSqlBackend, HasDataTypeCreatedCheck
                                       , BeamMigrateSqlBackendDataTypeSyntax, HasDefaultSqlDataType (..)
                                       , CheckedDatabaseEntity (..), CheckedDatabaseEntityDescriptor)

import           Control.Lens (makeLenses, (&), (.~), Lens', (^.))
import           Control.Monad (join)

import           Data.Int (Int32)
import           Data.Maybe (fromMaybe)
import           Data.Proxy (Proxy(..))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (LocalTime)

import           Text.Read (readMaybe)

data BeamMigrateAction
    = Apply | Revert | System
      deriving (Show, Read, Eq, Ord, Enum)

instance (BeamSqlBackend be, HasSqlEqualityCheck be Text) => HasSqlEqualityCheck be BeamMigrateAction

instance HasDefaultSqlDataType be Text => HasDefaultSqlDataType be BeamMigrateAction where
    defaultSqlDataType _ be key = defaultSqlDataType (Proxy @Text) be key

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be BeamMigrateAction where
    sqlValueSyntax = sqlValueSyntax . T.pack . show

instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be BeamMigrateAction where
    fromBackendRow = do
      m <- readMaybe . T.unpack <$> fromBackendRow
      case m of
        Nothing -> fail "Could not read BeamMigrateAction"
        Just a -> pure a

data BeamMigrateLogT f
    = BeamMigrateLog
    { _bmlId   :: C f Int32
    , _bmlName :: C f MigrationName
    , _bmlBranch :: C f (Maybe BranchName)
    , _bmlAction :: C f BeamMigrateAction
    , _bmlUser :: C f Text
    , _bmlDate :: C f LocalTime
    , _bmlNote :: C f Text
    , _bmlMigrateHash :: C f (Maybe Text)
    , _bmlMigrateVersion :: C f Int32
    } deriving (Generic, Beamable)

type BeamMigrateLog = BeamMigrateLogT Identity
deriving instance Show BeamMigrateLog

instance Table BeamMigrateLogT where
    data PrimaryKey BeamMigrateLogT f
        = BeamMigrateLogKey (C f Int32)
          deriving (Generic, Beamable)
    primaryKey = BeamMigrateLogKey <$> _bmlId

data BeamMigrateDb f
    = BeamMigrateDb
    { _bmdbLog :: f (TableEntity BeamMigrateLogT)
    } deriving Generic

instance Database be BeamMigrateDb

logEntries = _bmdbLog

makeLenses ''BeamMigrateLogT
makeLenses ''BeamMigrateDb

makeBeamMigrateDb :: BeamMigrateCliBackend be
                  => Text -> CheckedDatabaseSettings be BeamMigrateDb
makeBeamMigrateDb tblName =
    defaultMigratableDbSettings `withDbModification`
    dbModification { _bmdbLog = modifyCheckedTable (\_ -> tblName)
                                checkedTableModification
                                { _bmlId             = checkedFieldNamed "id"
                                , _bmlName           = checkedFieldNamed "name"
                                , _bmlBranch         = checkedFieldNamed "branch"
                                , _bmlAction         = checkedFieldNamed "action"
                                , _bmlUser           = checkedFieldNamed "user"
                                , _bmlDate           = checkedFieldNamed "date"
                                , _bmlNote           = checkedFieldNamed "note"
                                , _bmlMigrateHash    = checkedFieldNamed "hash"
                                , _bmlMigrateVersion = checkedFieldNamed "beam_migrate_version"
                                }
                   }

beamMigrateDb :: BeamMigrateCliBackend be
              => CheckedDatabaseSettings be BeamMigrateDb
beamMigrateDb = makeBeamMigrateDb "__beam_migrate__"

type BeamMigrateCliBackend be = ( HasDataTypeCreatedCheck (BeamMigrateSqlBackendDataTypeSyntax be)
                                , HasDefaultSqlDataType be LocalTime
                                , BeamMigrateSqlBackend be )


checkedEntityDescriptor :: Lens' (CheckedDatabaseEntity be db ety) (CheckedDatabaseEntityDescriptor be ety)
checkedEntityDescriptor f (CheckedDatabaseEntity e ps) = (\e' -> CheckedDatabaseEntity e' ps) <$> f e

orderedLogEntries :: BeamSqlBackend be => DatabaseSettings be BeamMigrateDb
                  -> Q be BeamMigrateDb s (QExprTable be s BeamMigrateLogT)
orderedLogEntries migrateDb =
  orderBy_ (\l -> desc_ (l ^. bmlId)) $
  all_ (_bmdbLog migrateDb)

logTableVersion1 = _bmdbLog

getNextEntryId :: (MonadBeam be m, BeamSqlBackend be, HasQBuilder be, FromBackendRow be Int32, FromBackendRow be SqlNull) => DatabaseSettings be BeamMigrateDb -> m Int32
getNextEntryId migrateDb =
    fmap (fromMaybe 0) $
    runSelectReturningOne $ select $
    aggregate_ (\l -> maybe_ 0 (\x -> x + val_ 1) (max_ (l ^. bmlId))) $
    all_ (_bmdbLog migrateDb)
