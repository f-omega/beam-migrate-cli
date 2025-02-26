{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Beam.Migrate.Cli.Database where

import           Database.Beam
import           Database.Beam.Backend (BeamBackend)
import           Database.Beam.Backend.SQL (HasSqlValueSyntax (..))
import           Database.Beam.Schema.Tables (dbEntityName)

import           Control.Lens (makeLenses, (&), (.~))

import           Data.Int (Int32)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (LocalTime)

import           Text.Read (readMaybe)

data BeamMigrateAction
    = Apply | Revert
      deriving (Show, Read, Eq, Ord, Enum)

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
    , _bmlName :: C f Text
    , _bmlAction :: C f BeamMigrateAction
    , _bmlUser :: C f Text
    , _bmlDate :: C f LocalTime
    , _bmlNote :: C f Text
    , _bmlMigrateHash :: C f Text
    , _bmlMigrateVersion :: C f Int32
    } deriving (Generic, Beamable)

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

makeLenses ''BeamMigrateLogT
makeLenses ''BeamMigrateDb

makeBeamMigrateDb :: Text -> DatabaseSettings be BeamMigrateDb
makeBeamMigrateDb tblName =
    defaultDbSettings `withDbModification`
    dbModification { _bmdbLog = setEntityName tblName
                             <> modifyTableFields tableModification
                                { _bmlId = "id"
                                , _bmlName = "name"
                                , _bmlAction = "action"
                                , _bmlUser = "user"
                                , _bmlDate = "date"
                                , _bmlNote = "note"
                                , _bmlMigrateHash = "hash"
                                , _bmlMigrateVersion = "beam_migrate_version"
                                }
                   }

beamMigrateDb :: DatabaseSettings be BeamMigrateDb
beamMigrateDb = makeBeamMigrateDb "__beam_migrate__"
