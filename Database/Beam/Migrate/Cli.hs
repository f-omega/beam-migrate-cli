module Database.Beam.Migrate.Cli
     ( beamMigrateCli
     , BeamMigrateOptions(..)
     , registerBackend
     , defBeamMigrateOptions
     , emptyDb, EmptyDb ) where

import           Database.Beam (MonadBeam)
import           Database.Beam.Migrate (CheckedDatabaseSettings)
import           Database.Beam.Migrate.Backend
import           Database.Beam.Migrate.Cli.Commands.Add (beamMigrateAdd)
import           Database.Beam.Migrate.Cli.Registry
import           Database.Beam.Migrate.Cli.Types
import           Database.Beam.Migrate.Cli.VC
import           Database.Beam.Migrate.Simple (haskellSchema)

import           Control.Exception (Exception, throwIO)
import           Control.Lens (makeLenses, (^.), (%~))

import           Data.IORef (newIORef)
import           Data.List (find, isPrefixOf)
import qualified Data.Text as T

import           Options.Applicative

import           System.Directory (doesFileExist)
import           System.FilePath (takeDirectory)

defBeamMigrateOptions :: BeamMigrateOptions
defBeamMigrateOptions = BeamMigrateOptions
                      { _beamMigrateCliDesc = "beam-migrate - Manage beam migrations"
                      , _beamMigrateProgName = "beam-migrate"
                      , _beamMigrateBackends = []
                      , _beamMigrateRunInDatabase = \_ -> pure Nothing
                      , _beamMigrateListDatabases = Nothing
                      , _beamMigrateAllowDirect = True
                      , _beamMigrateFindRegistry = defaultFindRegistry
                      , _beamMigrateFindMigrationsDir = defaultFindMigrationsDir
                      , _beamMigrateVcGetBranch = defaultVcGetBranch
                      }

cliOptsParser :: BeamMigrateOptions -> Parser BeamMigrateCliOptions
cliOptsParser bm =
    BeamMigrateCliOptions
      <$> flag True False (long "show-commands" <> help "Show commands as they're run")
      <*> verbosity
      <*> database
      <*> cliCommand

    where
      verbosity = silent <|> verbose <|> pure Normal
      silent = Silent <$ flag () () (long "quiet" <> short 'q' <> help "Only output necessary information")
      verbose = Verbose <$ flag () () (long "verbose" <> short 'v' <> help "Output copious information")

      database = connStr <|>
                 (NamedDb <$> strOption (long "database" <> short 'D' <> help "Name of database to connect to" <> metavar "NAME"))

      connStr
          | bm ^. beamMigrateAllowDirect =
              ConnStr <$> strOption (long "connstr" <> short 'C' <> help "Database connection string to use" <> metavar "CONNSTR")
          | otherwise = empty

      cliCommand = subparser . mconcat $
                   [ command "status" statusCmd
                   , command "log" logCmd
                   , command "add" addCmd
                   , command "genhs" genhsCmd
                   , command "migrate" migrateCmd
                   , command "revert" revertCmd
                   , command "verify" verifyCmd ]

      statusCmd = info (pure (Status StatusCmd)) (progDesc "Get status of migration registry")
      logCmd = info (pure (Log LogCmd)) (progDesc "Get status of migration registry")
      addCmd = info (Add <$> (AddCmd
                              <$> (MigrationName . T.pack <$> strArgument (metavar "NAME" <> help "Name of new migration"))
                              <*> many (MigrationName . T.pack <$> strArgument (metavar "DEP" <> help "Names of migrations this depends on. If not given, it will be inferred"))
                              <*> optional (T.pack <$> strOption (metavar "MESSAGE" <> help "Commit message to save with the migration" <> long "message" <> short 'm'))
                             )) (progDesc "Add a new migration to the registry")
      genhsCmd = info (pure (Genhs GenhsCmd)) (progDesc "Generate a haskell schema for this database")
      migrateCmd = info (pure (Migrate MigrateCmd)) (progDesc "Get status of migration registry")
      verifyCmd = info (pure (Verify VerifyCmd)) (progDesc "Get status of migration registry")
      revertCmd = info (pure (Revert RevertCmd)) (progDesc "Get status of migration registry")

beamMigrateCli :: BeamMigrateOptions -> IO ()
beamMigrateCli opts = do
  parsedOpts <- execParser (info (cliOptsParser opts <**> helper)
                            (progDesc (opts ^. beamMigrateCliDesc)))

  putStrLn ("Opts " ++ show parsedOpts)

  ctxt <- openBeamMigrateContext opts parsedOpts

  case cliCommand parsedOpts of
    Genhs _ -> do
      hsSchema <- runBeamMigration ctxt (\be _ -> haskellSchema be)
      putStrLn hsSchema

    Add cmd -> beamMigrateAdd ctxt cmd
    _ -> putStrLn "Not implemented"

registerBackend :: BeamMigrationBackend be m -> CheckedDatabaseSettings be db
                -> BeamMigrateOptions -> BeamMigrateOptions
registerBackend be db = beamMigrateBackends %~ (SomeCliMigrationBackend be db:)

runBeamMigration :: BeamMigrateContext -> (forall be db m. (MonadBeam be m, MonadFail m) => BeamMigrationBackend be m -> CheckedDatabaseSettings be db -> m a) -> IO a
runBeamMigration bmc go =
    case bmc ^. bmcRunner of
      BeamDatabaseRunner be@BeamMigrationBackend {} db run ->
        run (go be db) >>=
        \case
           Left e -> throwIO (BeamMigrateDdl e)
           Right x -> pure x

openBeamMigrateContext :: BeamMigrateOptions -> BeamMigrateCliOptions -> IO BeamMigrateContext
openBeamMigrateContext opts cli = do
  mRunner <- case cliDatabase cli of
               NamedDb nm -> (opts ^. beamMigrateRunInDatabase) nm
               ConnStr conn ->
                   case find (isBackendConnStr conn) (opts ^. beamMigrateBackends) of
                     Nothing -> pure Nothing
                     Just (SomeCliMigrationBackend be@BeamMigrationBackend { backendTransact = transact } db) ->
                         let conn' = drop (length (backendName be) + 1) conn
                         in pure (Just (BeamDatabaseRunner be db (transact conn')))

  runner <- case mRunner of
              Nothing -> fail "Could not open database"
              Just runner -> pure runner

  -- Read registry
  registryFile <- opts ^. beamMigrateFindRegistry
  registryExists <- doesFileExist registryFile
  registry <- if registryExists then readRegistry registryFile else pure newRegistry

  migrationsDir <- (opts ^. beamMigrateFindMigrationsDir) registryFile

  registryVar <- newIORef (newRegistryStatus registry)

  pure BeamMigrateContext { _bmcRunner = runner
                          , _bmcDatabase = cliDatabase cli
                          , _bmcOpts = opts
                          , _bmcRegistryRef = registryVar
                          , _bmcTopDir = takeDirectory registryFile
                          , _bmcMigrationsDir = migrationsDir
                          , _bmcVerbosity = cliVerbosity cli }
  where
    isBackendConnStr :: String -> SomeCliMigrationBackend -> Bool
    isBackendConnStr conn (SomeCliMigrationBackend BeamMigrationBackend { backendName = nm } _) = (nm ++ ":") `isPrefixOf` conn
