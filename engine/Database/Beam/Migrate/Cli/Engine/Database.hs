{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}

module Database.Beam.Migrate.Cli.Engine.Database where

import           Database.Beam
import           Database.Beam.Backend (BeamBackend, BeamSqlBackend, BeamSqlBackendSupportsDataType, SqlNull)
import           Database.Beam.Backend.SQL (HasSqlValueSyntax (..))
import           Database.Beam.Migrate ( CheckedDatabaseSettings, defaultMigratableDbSettings
                                       , renameCheckedEntity, modifyCheckedTable, checkedTableModification
                                       , checkedFieldNamed, BeamMigrateSqlBackend, HasDataTypeCreatedCheck
                                       , BeamMigrateSqlBackendDataTypeSyntax, HasDefaultSqlDataType (..)
                                       , CheckedDatabaseEntity (..), CheckedDatabaseEntityDescriptor
                                       , SomeDatabasePredicate(..), TableExistsPredicate(..)
                                       , QualifiedName(..) )
import           Database.Beam.Migrate.Backend (BeamMigrationBackend(..))
import           Database.Beam.Schema.Tables (dbEntityName, dbEntityDescriptor, dbEntitySchema)

import           Control.Lens (makeLenses, (&), (.~), Lens', (^.))
import           Control.Monad (join)

import           Data.Int (Int32)
import           Data.Maybe (fromMaybe)
import           Data.Proxy (Proxy(..))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (LocalTime)
import           Data.Time (ZonedTime(zonedTimeToLocalTime), getZonedTime)

import           Text.Read (readMaybe)

newtype MigrationName = MigrationName Text deriving (Show, Eq, Ord)
deriving newtype instance HasSqlValueSyntax be Text => HasSqlValueSyntax be MigrationName
deriving newtype instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be MigrationName
deriving newtype instance HasDefaultSqlDataType be Text => HasDefaultSqlDataType be MigrationName
deriving newtype instance (BeamSqlBackend be,  HasSqlEqualityCheck be Text) => HasSqlEqualityCheck be MigrationName

newtype BranchName = BranchName Text
    deriving (Show, Eq)
deriving newtype instance HasSqlValueSyntax be Text => HasSqlValueSyntax be BranchName
deriving newtype instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be BranchName
deriving newtype instance HasDefaultSqlDataType be Text => HasDefaultSqlDataType be BranchName

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

migrateTableExistsPredicate :: BeamMigrationBackend be m -> DatabaseSettings be BeamMigrateDb -> SomeDatabasePredicate
migrateTableExistsPredicate BeamMigrationBackend {} db =
    SomeDatabasePredicate $
    TableExistsPredicate (QualifiedName (db ^. bmdbLog . dbEntityDescriptor . dbEntitySchema)
                                        (db ^. bmdbLog . dbEntityDescriptor . dbEntityName))

getLastBeamMigrateVersion' :: HasSqlEqualityCheck be Text
                           => BeamMigrationBackend be m -> DatabaseSettings be BeamMigrateDb -> m (Maybe Int32)
getLastBeamMigrateVersion' BeamMigrationBackend {} migrateDb =
  runSelectReturningOne $
    select $ fmap (\l -> l ^. bmlMigrateVersion) $
    orderBy_ (\l -> desc_ (l ^. bmlId)) $
    filter_ (\l -> l ^. bmlAction ==. val_ System) $
    all_ (_bmdbLog migrateDb) -- Not using orderedLogEntries on  purpose

insertBeamMigrationVersion :: MonadIO m => BeamMigrationBackend be m -> DatabaseSettings be BeamMigrateDb
                           -> Text -> Int32 -> m (SqlInsert be BeamMigrateLogT)
insertBeamMigrationVersion BeamMigrationBackend {} migrateDb user version = do
  today <- zonedTimeToLocalTime <$> liftIO getZonedTime
  entryId <- getNextEntryId migrateDb
  pure (insert (migrateDb ^. bmdbLog)
       $ insertExpressions
        [ BeamMigrateLog { _bmlId = val_ entryId
                         , _bmlName = val_ (MigrationName "beam-migrate")
                         , _bmlBranch = nothing_
                         , _bmlAction = val_ System
                         , _bmlUser = val_ user
                         , _bmlDate = val_ today
                         , _bmlNote = val_ ""
                         , _bmlMigrateHash = nothing_
                         , _bmlMigrateVersion = val_ version } ])

listAllAppliedMigrations :: HasSqlEqualityCheck be Text
  => BeamMigrationBackend be m -> DatabaseSettings be BeamMigrateDb -> m [MigrationName]
listAllAppliedMigrations BeamMigrationBackend{} migrateDb =
    runSelectReturningList $ select $ nub_ $ do
      entry <- all_ (logEntries migrateDb)
      guard_ $ entry ^. bmlAction ==. val_ Apply
      guard_ $ not_ $ exists_ $ do
        reversionEntry <- all_ (logEntries migrateDb)
        guard_ (reversionEntry ^. bmlId >. entry ^. bmlId &&.
                reversionEntry ^. bmlName ==. entry ^. bmlName &&.
                reversionEntry ^. bmlAction ==. val_ Revert)
        pure reversionEntry
      pure (entry ^. bmlName)
