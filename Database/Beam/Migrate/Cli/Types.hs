{-# LANGUAGE UndecidableInstances #-}

module Database.Beam.Migrate.Cli.Types where

import Database.Beam
import Database.Beam.Migrate
import Database.Beam.Migrate (defaultMigratableDbSettings, BeamMigrateSqlBackend, HasDataTypeCreatedCheck, BeamMigrateSqlBackendDataTypeSyntax)
import Database.Beam.Migrate.Backend
import Database.Beam.Migrate.Cli.Registry (Registry, MigrationName, BranchName)
import Database.Beam.Migrate.Types (CheckedDatabaseSettings)

import Control.Exception (Exception)
import Control.Lens (makeLenses, (^.))

import Data.ByteString (ByteString)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Int (Int32)
import Data.Kind (Type)
import Data.String (fromString)
import Data.Text (Text)

data BeamMigrateError
     = BeamMigrateDdl DdlError
       deriving (Show, Exception)

data BeamDatabaseRunner where
    BeamDatabaseRunner :: BeamMigrationBackend be m -> CheckedDatabaseSettings be db -> (forall a. m a -> IO (Either DdlError a)) -> BeamDatabaseRunner

data SomeCliMigrationBackend where
    SomeCliMigrationBackend :: BeamMigrationBackend be m -> CheckedDatabaseSettings be db -> SomeCliMigrationBackend

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
    }

data DatabaseName = ConnStr String | NamedDb String deriving Show
data BeamMigrateContext
    = BeamMigrateContext
    { _bmcRunner :: !BeamDatabaseRunner
    , _bmcDatabase :: !DatabaseName
    , _bmcOpts :: !BeamMigrateOptions
    , _bmcRegistryRef :: !(IORef RegistryStatus)
    , _bmcTopDir :: !FilePath
    , _bmcMigrationsDir :: !FilePath
    , _bmcVerbosity :: !Verbosity
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

data StatusCmd = StatusCmd deriving Show
data LogCmd = LogCmd deriving Show
data AddCmd
    = AddCmd
    { _addMigrationName :: !MigrationName
    , _addMigrationDeps :: [MigrationName]
    , _addCommitMessage :: Maybe Text
    }
    deriving Show
data MigrateCmd = MigrateCmd deriving Show
data VerifyCmd = VerifyCmd deriving Show
data RevertCmd = RevertCmd deriving Show
data GenhsCmd = GenhsCmd deriving Show

data CliCommand
    = Status StatusCmd
    | Log LogCmd
    | Add AddCmd
    | Genhs GenhsCmd

    | Migrate MigrateCmd
    | Revert  RevertCmd
    | Verify VerifyCmd
    deriving Show

makeLenses ''AddCmd

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
