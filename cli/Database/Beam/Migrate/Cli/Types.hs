{-# LANGUAGE UndecidableInstances #-}

module Database.Beam.Migrate.Cli.Types where

import Database.Beam
import Database.Beam.Migrate
import Database.Beam.Migrate (defaultMigratableDbSettings, BeamMigrateSqlBackend, HasDataTypeCreatedCheck, BeamMigrateSqlBackendDataTypeSyntax)
import Database.Beam.Migrate.Backend
import Database.Beam.Migrate.Cli.Engine.Internal (BeamMigrateDb, MigrationName, BranchName)
import Database.Beam.Migrate.Cli.Registry (Registry)
import Database.Beam.Migrate.Types (CheckedDatabaseSettings)

import Control.Exception (Exception)
import Control.Lens (makeLenses, (^.))

import Data.ByteString (ByteString)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Int (Int32)
import Data.Kind (Type)
import Data.String (fromString, IsString)
import Data.Time (LocalTime)
import Data.Text (Text)

data BeamMigrateError
     = BeamMigrateDdl DdlError
       deriving (Show, Exception)

data BeamDatabaseRunner where
    BeamDatabaseRunner
        :: (MonadIO m, HasSqlEqualityCheck be Text, HasDefaultSqlDataType be LocalTime, Database be db)
        => { bdbrBackend :: BeamMigrationBackend be m
           , bdbrDatabase :: CheckedDatabaseSettings be db
           , bdbrMigrateDb :: BeamMigrateDbSettings be
           , bdbrRun :: (forall a. m a -> IO (Either DdlError a))
           , bdbrClose :: IO ()
           } -> BeamDatabaseRunner

data SomeCliMigrationBackend where
    SomeCliMigrationBackend :: ( HasSqlEqualityCheck be Text, MonadIO m, HasDefaultSqlDataType be LocalTime
                               , Database be db )
                            => BeamMigrationBackend be m -> CheckedDatabaseSettings be db
                            -> BeamMigrateDbSettings be
                            -> SomeCliMigrationBackend

data BeamMigrateDbSettings be
    = BeamMigrateDbCheckable (CheckedDatabaseSettings be BeamMigrateDb)
    | BeamMigrateDbUncheckable (DatabaseSettings be BeamMigrateDb)

queryableMigrateDb :: BeamMigrateDbSettings be -> DatabaseSettings be BeamMigrateDb
queryableMigrateDb (BeamMigrateDbUncheckable d) = d
queryableMigrateDb (BeamMigrateDbCheckable d) = unCheckDatabase d

data BeamMigrateOptions
    = BeamMigrateOptions
    { _beamMigrateProgName :: String
    , _beamMigrateCliDesc :: String
    , _beamMigrateBackends  :: [SomeCliMigrationBackend]

    , _beamMigrateRunInDatabase :: String -> IO (Maybe BeamDatabaseRunner)
    , _beamMigrateListDatabases :: Maybe (IO [String])

    -- | Whether to allow directly given connection strings, or only named ones
    , _beamMigrateAllowDirect :: Bool

    , _beamMigrateFindRegistry :: IO FilePath
    , _beamMigrateFindMigrationsDir :: FilePath -> IO FilePath
    , _beamMigrateVcGetBranch  :: BeamMigrateContext -> IO (Maybe BranchName)

    , _beamMigrateGetCurrentUser :: IO Text
    }

data DatabaseName = ConnStr String | NamedDb String | NoDb deriving Show
data BeamMigrateContext
    = BeamMigrateContext
    { _bmcRunner :: !(Maybe BeamDatabaseRunner)
    , _bmcDatabase :: !DatabaseName
    , _bmcOpts :: !BeamMigrateOptions
    , _bmcRegistryRef :: !(IORef RegistryStatus)
    , _bmcRegistryFile :: !FilePath
    , _bmcTopDir :: !FilePath
    , _bmcMigrationsDir :: !FilePath
    , _bmcVerbosity :: !Verbosity
    , _bmcExeName :: !String
    , _bmcDumpSql :: !Bool
    }

data RegistryStatus = RegistryStatus !Registry !(Maybe Registry)
                      deriving Show

newRegistryStatus :: Registry -> RegistryStatus
newRegistryStatus reg = RegistryStatus reg Nothing

getCurrentRegistry :: RegistryStatus -> Registry
getCurrentRegistry (RegistryStatus reg Nothing) = reg
getCurrentRegistry (RegistryStatus _ (Just reg)) = reg

updateRegistryStatus :: RegistryStatus -> Registry -> RegistryStatus
updateRegistryStatus (RegistryStatus reg _) new = RegistryStatus reg (Just new)

registryNeedsWrite :: RegistryStatus -> Bool
registryNeedsWrite (RegistryStatus _ Just {}) = True
registryNeedsWrite _ = False

data Verbosity = Silent | Normal | Verbose
                 deriving (Show, Eq, Ord)

makeLenses ''BeamMigrateContext
makeLenses ''BeamMigrateOptions

readCurrentRegistry :: BeamMigrateContext -> IO Registry
readCurrentRegistry bmc = getCurrentRegistry <$> readIORef (bmc ^. bmcRegistryRef)

modifyContextRegistry :: BeamMigrateContext -> (Registry -> IO (a, Registry)) -> IO a
modifyContextRegistry ctxt go = do
  sts <- readIORef (ctxt ^. bmcRegistryRef)
  (x, r') <- go (getCurrentRegistry sts)
  writeIORef (ctxt ^. bmcRegistryRef) (updateRegistryStatus sts r')
  pure x

modifyContextRegistry' :: BeamMigrateContext -> (Registry -> IO Registry) -> IO ()
modifyContextRegistry' ctxt go =
    modifyContextRegistry ctxt (fmap ((,) ()) . go)

data BeamMigrateCliOptions
    = BeamMigrateCliOptions
    { cliShowCommands :: !Bool
    , cliVerbosity    :: !Verbosity
    , cliDatabase     :: !DatabaseName
    , cliCommand      :: !CliCommand
    } deriving Show

newtype DbSource = DbSource Text
    deriving newtype (IsString, Show)

data StatusCmd
    = StatusCmd
    { _statusPorcelain :: !Bool
    , _statusForceDbStatus :: !Bool
    -- ^ Force a status check on the db even if the table is not up to date
    }
    deriving Show
data LogCmd
    = LogCmd
    { _logShort :: !Bool
    } deriving Show
data AddCmd
    = AddCmd
    { _addMigrationName :: !MigrationName
    , _addMigrationDeps :: [MigrationName]
    , _addCommitMessage :: Maybe Text
    }
    deriving Show
data AbortCmd
    = AbortCmd
    { _abortBranchName :: !(Maybe BranchName)
    , _abortForce      :: !Bool
    , _abortKeep       :: !Bool
    , _abortKeepFiles  :: !Bool
    } deriving Show
data MigrateCmd
    = MigrateCmd
    { _migrateStaged :: !Bool
    , _migrateDryRun :: !Bool
    , _migrateTest   :: !Bool
    -- ^ Whether to just test the migrations (abort transaction at end)
    , _migrateNote   :: Text
    } deriving Show
data VerifyCmd
    = VerifyCmd
    { _verifyIgnoreInternal :: !Bool
      -- ^ If set, don't use the internal beam checks, only the external script
    , _verifySource         :: !DbSource
    }
    deriving Show
data DumpCmd
    = DumpCmd
    { _dumpSources :: [DbSource]
    , _dumpJson :: Bool
    } deriving Show
data InitDbCmd
    = InitDbCmd
    { _initDbDryRun :: !Bool
    , _initDbForce :: !Bool
    , _initDbRepair :: !Bool
    } deriving Show

data StatusDbCmd
    = StatusDbCmd
    { _statusDbForce :: !Bool
    } deriving Show

data EditCmd
    = EditCmd
    { _editMigration :: !MigrationName
    , _editFile :: !MigrateFile
    } deriving Show

data CommitCmd
    = CommitCmd
    { _commitForce :: !Bool
    } deriving Show

data DiffFormatSpec = HaskellFormat | SourceFormat | DestinationFormat | SomeFormat deriving (Show, Eq, Ord)
data DiffCmd
    = DiffCmd
    { _diffFormat :: !DiffFormatSpec
    , _diffDump   :: !Bool
    , _diffSource :: !DbSource
    , _diffDest   :: !DbSource
    } deriving Show

data BranchSelector
    = SelectAllBranches
    | SelectNamedBranches [BranchName]
    | SelectDefaultBranch
      deriving Show

data BackendSelector
    = SelectAllBackends
    | SelectNamedBackends [Text]
      deriving Show

data BranchStatusSelector
    = IncludeWorking
    | LastCommit
      -- ^ Choose the last committed (default)
    | CommittedOnly
      -- ^ Enforce that each branch have no working migration
      deriving Show

data PickleCmd
    = PickleCmd
    { _pickleModuleName :: !Text
    , _pickleBranches   :: !BranchSelector
    , _pickleExtraMigrations :: [MigrationName]
    , _pickleObfuscate :: !Bool
    -- ^ If true, obfuscate branch names
    , _pickleBackends :: !BackendSelector
    -- ^ Which backends to choose
    , _pickleBranchStatus :: !BranchStatusSelector
    , _pickleTip :: Maybe BranchName
    } deriving Show

data CliCommand
    = Status StatusCmd
    | Log LogCmd
    | Add AddCmd
    | Abort AbortCmd
    | Dump DumpCmd

    | ManageDb ManageDb

    | Migrate MigrateCmd
    | Commit CommitCmd
    | Verify VerifyCmd

    | Edit EditCmd
    | Diff DiffCmd

    | Pickle PickleCmd
    deriving Show

data ManageDb
    = InitDb   !InitDbCmd
    | StatusDb !StatusDbCmd
      deriving Show

data MigrateFile
    = ApplyScript
    | RevertScript
    | VerifyScript
    | SchemaFile
      deriving (Show, Eq, Ord)

makeLenses ''AbortCmd
makeLenses ''DumpCmd
makeLenses ''AddCmd
makeLenses ''CommitCmd
makeLenses ''DiffCmd
makeLenses ''EditCmd
makeLenses ''InitDbCmd
makeLenses ''LogCmd
makeLenses ''MigrateCmd
makeLenses ''PickleCmd
makeLenses ''StatusCmd
makeLenses ''StatusDbCmd
makeLenses ''VerifyCmd

-- Empty database

data TestTable f = TestTable { testColumn1 :: C f Int32 } deriving (Generic, Beamable)
instance Table TestTable where
    data PrimaryKey TestTable f = TestKey (C f Int32) deriving (Generic, Beamable)
    primaryKey = TestKey <$> testColumn1

data EmptyDb (entity :: Type -> Type) = EmptyDb (entity (TableEntity TestTable)) deriving (Generic)

instance BeamMigrateSqlBackend be => Database be EmptyDb

emptyDb :: (BeamMigrateSqlBackend be, HasDataTypeCreatedCheck (BeamMigrateSqlBackendDataTypeSyntax be))
        => CheckedDatabaseSettings be EmptyDb
emptyDb = defaultMigratableDbSettings

-- * Templates for editing

newtype Template = Template ByteString deriving Show

mkTemplate :: [String] -> Template
mkTemplate = Template . fromString . unlines

-- * Migration schema

data MigrationSchema
    = MigrationSchema
    { _msStartingSchema, _msEndingSchema :: DatabasePoint
    } deriving Show

newtype DatabasePoint = DatabasePoint [SomeDatabasePredicate]
    deriving Show

makeLenses ''MigrationSchema

initialDatabasePoint :: DatabasePoint
initialDatabasePoint = DatabasePoint []

emptyMigrationSchema :: MigrationSchema
emptyMigrationSchema = MigrationSchema { _msStartingSchema = initialDatabasePoint
                                       , _msEndingSchema = initialDatabasePoint }
