{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Beam.Migrate.Cli.Commands.Common where

import           Database.Beam
import           Database.Beam.Backend (BeamSqlBackendSyntax)
import           Database.Beam.Migrate (SomeDatabasePredicate(..), serializePredicate, DatabasePredicate (englishDescription, predicateCascadesDropOn), collectChecks)
import           Database.Beam.Migrate.Backend (DdlError, BeamMigrationBackend (..), BeamMigrateConnection (..), SomeBeamMigrationBackend (..))
import           Database.Beam.Migrate.Cli.Engine.Internal
import           Database.Beam.Migrate.Cli.Message
import           Database.Beam.Migrate.Cli.Registry
import           Database.Beam.Migrate.Cli.Types hiding (Revert)
import           Database.Beam.Migrate.Serialization (beamDeserialize)
import           Database.Beam.Migrate.Simple (VerificationResult, verifySchema)

import           Control.Applicative (empty)
import           Control.Exception (Exception, throwIO, SomeException (..), catch, bracket, throw)
import           Control.Lens ((^.), has, _2, view, preview, _1, _Just)
import           Control.Monad (when)
import           Control.Monad.Reader (runReaderT, ReaderT(..), ask, MonadTrans)
import           Control.Monad.Trans (MonadTrans, lift)

import           Crypto.Hash (hashWith, SHA512 (SHA512))

import qualified Data.Aeson as JSON
import qualified Data.Aeson.KeyMap as JSON
import qualified Data.Aeson.Types as JSON (parseMaybe)
import           Data.ByteArray.Encoding (convertToBase, Base(Base16))
import           Data.Foldable (foldrM)
import qualified Data.HashSet as HS
import           Data.List (partition, isPrefixOf, find)
import           Data.Maybe (mapMaybe)
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as T

import           System.FilePath ((</>))
import           System.IO (hPutStrLn, stderr)
import           System.Directory (createDirectoryIfMissing, removeFile, removeDirectory)

import           Text.Editor (runUserEditorDWIM, plainTemplate)

cmdline :: BeamMigrateContext -> Message -> Message
cmdline ctx args = bolden ("'" <> formatExeName (fromString (ctx ^. bmcExeName)) <> " " <> args <> "'")

getRunner :: BeamMigrateContext -> IO BeamDatabaseRunner
getRunner ctx =
    case ctx ^. bmcRunner of
      Nothing -> noDatabaseError ctx
      Just runner -> pure runner

noDatabaseError :: BeamMigrateContext -> a
noDatabaseError ctx = beamMigrateError ctx "No database present in context. Use the --connstr or --database options"

whenVerbose :: Applicative m => BeamMigrateContext -> m () -> m ()
whenVerbose c = when (isVerbose c)

isVerbose :: BeamMigrateContext -> Bool
isVerbose c =
    case c ^. bmcVerbosity of
      Verbose -> True
      _ -> False

beamMigrateError :: BeamMigrateContext -> Message -> a
beamMigrateError _ s =
  throw (BeamMigrateError s)

beamMigrateMessage :: BeamMigrateContext -> Message -> IO ()
beamMigrateMessage _ = hPutMessageLn stderr

beamMigrateOutput :: BeamMigrateContext -> Message -> IO ()
beamMigrateOutput _ = putMessageLn

beamMigrateInfo :: BeamMigrateContext -> Message -> IO ()
beamMigrateInfo ctxt s =
    case ctxt ^. bmcVerbosity of
      Silent -> pure ()
      _ -> beamMigrateMessage ctxt s

getCurrentBranch :: BeamMigrateContext -> IO (Maybe BranchName)
getCurrentBranch ctxt = (ctxt ^. bmcOpts . beamMigrateVcGetBranch) ctxt

getCurrentBranchOrDie :: BeamMigrateContext -> IO BranchName
getCurrentBranchOrDie ctxt = do
  mBranch <- getCurrentBranch ctxt
  case mBranch of
    Nothing -> beamMigrateError ctxt "Could not determine the name of the VC branch. Are you in a version-controlled directory?"
    Just branch -> pure branch

updateMigration :: BeamMigrateContext -> MigrationInfo -> IO ()
updateMigration ctxt mig =
  modifyContextRegistry' ctxt (\reg -> pure (updateMigrationInRegistry mig reg))

unregisterMigration :: BeamMigrateContext -> MigrationName -> IO ()
unregisterMigration ctxt nm =
  modifyContextRegistry' ctxt (\reg -> pure (removeMigrationFromRegistry nm reg))

getLatestCommittedMigration, getLatestMigration
    :: BeamMigrateContext -> BranchName -> IO (Maybe (MigrationName, BranchStatus))

getLatestCommittedMigration ctxt branchName = do
  registry <- readCurrentRegistry ctxt
  let committed = filter (has (_2 . branchStatusStatus . _Committed)) (findMigrationsForBranch registry branchName)
  case committed of
    [] -> pure Nothing
    [(mi, sts)] -> pure (Just (mi ^. miName, sts))
    xs -> needsMergeError ctxt branchName xs

getLatestMigration ctxt branchName = do
  registry <- readCurrentRegistry ctxt
  let (committed, working) = partition (has (_2 . branchStatusStatus . _Committed)) (findMigrationsForBranch registry branchName)
  case working of
    [] -> case committed of
            [] -> pure Nothing
            [(mi, sts)] -> pure (Just (mi ^. miName, sts))
            xs -> needsMergeError ctxt branchName xs
    [(mi, sts)] -> pure (Just (mi ^. miName, sts))
    xs -> needsMergeError ctxt branchName xs

getMigration :: BeamMigrateContext -> MigrationName -> IO (Maybe MigrationInfo)
getMigration ctxt migName = do
  registry <- readCurrentRegistry ctxt
  pure (lookupMigration migName registry)

needsMergeError :: BeamMigrateContext -> BranchName -> [(MigrationInfo, BranchStatus)] -> IO a
needsMergeError ctxt nm tips =
    beamMigrateError ctxt ("Branch " <> pretty nm <> " needs merge.\n" <>
                           pretty (length tips) <> " migrations are currently recorded as its tip.\n" <>
                           foldMap reportTip tips)
  where
    reportTip (mi, BranchStatus sts _) =
        "  - " <> case sts of { Working -> "* "; _ -> "" } <> pretty (mi ^. miName) <> "\n"

getOrEditMessage :: BeamMigrateContext -> Template -> Maybe Text -> IO Text
getOrEditMessage _ _ (Just x) = pure x
getOrEditMessage _ (Template x) Nothing = stripMessage . TE.decodeUtf8 <$> runUserEditorDWIM plainTemplate x

stripMessage :: Text -> Text
stripMessage = T.unlines . dropEmpty . filter (not . T.isPrefixOf (T.pack "#")) . T.lines
  where
    dropEmpty = reverse . dropWhile T.null . reverse . dropWhile T.null

-- * Files

migrateFileName :: MigrateFile -> FilePath
migrateFileName ApplyScript = "apply.sql"
migrateFileName RevertScript = "revert.sql"
migrateFileName VerifyScript = "verify.sql"
migrateFileName SchemaFile = "beam_schema"

data CanonicalFile
    = MigrateFile !MigrationName !MigrateFile
    | MigrationDir !MigrationName
      deriving (Show, Eq, Ord)

canonicalFileName :: CanonicalFile -> FilePath
canonicalFileName (MigrateFile mig file) =
  canonicalFileName (MigrationDir mig) </> migrateFileName file
canonicalFileName (MigrationDir (MigrationName mig)) =
  T.unpack mig

fullFilePath :: BeamMigrateContext -> CanonicalFile -> FilePath
fullFilePath ctxt fl@(MigrateFile {}) =
    ctxt ^. bmcMigrationsDir </> canonicalFileName fl
fullFilePath ctxt fl@(MigrationDir nm) =
    ctxt ^. bmcMigrationsDir </> canonicalFileName fl

notifyWriteFile :: BeamMigrateContext -> CanonicalFile -> String -> IO ()
notifyWriteFile ctxt fl contents = do
  let fp = fullFilePath ctxt fl
  beamMigrateInfo ctxt ("Write file " <> filename fp <> "...")
  writeFile fp contents

commitMessageTemplate :: MigrationName -> Template
commitMessageTemplate (MigrationName nm) =
    mkTemplate
    [ "Commit message for '" <> T.unpack nm <> "'"
    , ""
    , "# Please type your commit message for this migration above."
    , "# A commit message can be multiple lines. The first line should"
    , "# be a concise descriptnio of the change."
    , ""
    , "# Some notes:"
    , "#"
    , "#   * Lines beginning with a '#' are treated as comments and ignored"
    , "#   * Empty lines at the beginning and end of the commit message"
    , "#     will be ignored"
    ]

ensureMigrationsDir :: BeamMigrateContext -> IO ()
ensureMigrationsDir bmc = createDirectoryIfMissing True (bmc ^. bmcMigrationsDir)

type SerializedMigrationSchema = JSON.KeyMap [JSON.Value]

readMigrationSchema :: BeamMigrateContext -> MigrationName -> IO MigrationSchema
readMigrationSchema ctxt nm = do
  schema :: SerializedMigrationSchema <-
    maybe (beamMigrateError ctxt ("Could not parse migration schema for " <> pretty nm)) pure =<<
    JSON.decodeFileStrict (fullFilePath ctxt (MigrateFile nm SchemaFile))

  starting <- case JSON.lookup "starting_schema" schema of
                Nothing -> beamMigrateError ctxt ("Could not read migration schema for " <> pretty nm <> ": starting_schema key not found")
                Just sch -> pure (parseDatabasePoint ctxt sch)

  ending <- case JSON.lookup "ending_schema" schema of
              Nothing -> beamMigrateError ctxt ("Could not read migration schema for " <> pretty nm <> ": ending_schema key not found")
              Just sch -> pure (parseDatabasePoint ctxt sch)

  pure MigrationSchema { _msStartingSchema = starting
                       , _msEndingSchema = ending }

parseDatabasePoint :: BeamMigrateContext -> [JSON.Value] -> DatabasePoint
parseDatabasePoint bmc vs =
  case bmc ^. bmcRunner of
    Just BeamDatabaseRunner { bdbrBackend = be } ->
        parseDatabasePointForBackend be vs
    Nothing -> error "No backend provided in this database"

parseDatabasePointForBackend :: BeamMigrationBackend be m -> [JSON.Value] -> DatabasePoint
parseDatabasePointForBackend BeamMigrationBackend { backendPredicateParsers = ds } vs =
    DatabasePoint $ mapMaybe (JSON.parseMaybe (beamDeserialize ds)) vs

writeMigrationSchema :: BeamMigrateContext -> MigrationName -> MigrationSchema -> IO ()
writeMigrationSchema ctxt nm ms = do
  let starting = serializeDatabasePoint ctxt (ms ^. msStartingSchema)
      ending = serializeDatabasePoint ctxt (ms ^. msEndingSchema)

  JSON.encodeFile (fullFilePath ctxt (MigrateFile nm SchemaFile)) $
      JSON.object [ "starting_schema" JSON..= starting
                  , "ending_schema"   JSON..= ending ]

serializeDatabasePoint :: BeamMigrateContext -> DatabasePoint -> [JSON.Value]
serializeDatabasePoint ctxt (DatabasePoint cs) = map (\(SomeDatabasePredicate p) -> serializePredicate p) cs

deleteMigration :: BeamMigrateContext -> MigrationInfo -> IO ()
deleteMigration ctxt mi = do
  let nm = mi ^. miName
  deleteFileNoFail ctxt (fullFilePath ctxt (MigrateFile nm SchemaFile))
  deleteEditedFileNoFail ctxt (fullFilePath ctxt (MigrateFile nm ApplyScript))
  deleteEditedFileNoFail ctxt (fullFilePath ctxt (MigrateFile nm RevertScript))
  deleteEditedFileNoFail ctxt (fullFilePath ctxt (MigrateFile nm VerifyScript))

  deleteDirectoryNoFail ctxt (fullFilePath ctxt (MigrationDir nm))

deleteEditedFileNoFail, deleteDirectoryNoFail, deleteFileNoFail :: BeamMigrateContext -> FilePath -> IO ()
deleteDirectoryNoFail ctxt fp = do
  removeDirectory fp `catch` (\e@(SomeException _) -> beamMigrateInfo ctxt ("Could not remove directory " <> filename fp <> ": " <> pretty e))
deleteFileNoFail ctxt fp = do
  deleted <- (True <$ removeFile fp) `catch` (\(SomeException {}) -> beamMigrateInfo ctxt ("Skipping removal of file " <> filename fp) >> pure False)
  when deleted (beamMigrateInfo ctxt ("Deleted file " <> filename fp))
deleteEditedFileNoFail ctxt fp = do
  deleteFileNoFail ctxt fp
#ifdef unix
  removeFile (fp <> "~") `catch` (\(SomeException {}) -> pure ())
#endif

printStatusShort :: BeamMigrateContext -> IO ()
printStatusShort ctxt = do
  branch <- getCurrentBranchOrDie ctxt
  mMigration <- getLatestMigration ctxt branch
  case mMigration of
    Nothing -> do beamMigrateInfo ctxt (warning "No migrations available")
                  beamMigrateInfo ctxt ("  at branch " <> pretty branch)
    Just (migrationName, BranchStatus sts fromBranch) -> do
      case sts of
        Committed -> beamMigrateInfo ctxt (status ("Now at committed migration '" <> pretty migrationName <> "'"))
        Working -> beamMigrateInfo ctxt (status ("Now at unstaged migration '" <> pretty migrationName <> "'"))

      if fromBranch == branch
         then beamMigrateInfo ctxt ("  at branch " <> pretty branch)
         else beamMigrateInfo ctxt ("  at branch " <> pretty branch <> " (forked from " <> pretty fromBranch <> ")")

checkMigrateDb :: BeamMigrateContext -> IO (Maybe VerificationResult)
checkMigrateDb ctx = do
  BeamDatabaseRunner { bdbrBackend = backend@BeamMigrationBackend {}
                     , bdbrRun = run, bdbrMigrateDb = migrateDb } <- getRunner ctx
  case migrateDb of
    BeamMigrateDbUncheckable {} -> pure Nothing
    BeamMigrateDbCheckable checked ->
        Just <$> dieOnDdlError ctx (run (verifySchema backend checked))

hushDdlError :: BeamMigrateContext -> IO (Either DdlError a) -> IO (Maybe a)
hushDdlError ctx go =
    go >>=
    \x -> case x of
            Left e -> whenVerbose ctx (beamMigrateMessage ctx (warning (fromString e))) >> pure Nothing
            Right x -> pure (Just x)

dieOnDdlError :: BeamMigrateContext -> IO (Either DdlError a) -> IO a
dieOnDdlError ctx go =
    go >>=
    \x -> case x of
            Left err -> beamMigrateError ctx ("SQL failure: " <> fromString err)
            Right x -> pure x

reportMissingChecks :: BeamMigrateContext -> [SomeDatabasePredicate] -> Message
reportMissingChecks ctx missing =
    vcat (map ((" - " <>) . formatMissing) missing)
  where
    formatMissing (SomeDatabasePredicate p) = fromString (englishDescription p)

getLatestLogEntry :: BeamMigrateContext -> IO (Maybe BeamMigrateLog)
getLatestLogEntry ctx = do
  BeamDatabaseRunner { bdbrBackend = BeamMigrationBackend {}
                     , bdbrRun = run, bdbrMigrateDb = migrateDb' } <- getRunner ctx
  let migrateDb = queryableMigrateDb migrateDb'
  dieOnDdlError ctx $ run $ do
    runSelectReturningFirst $ select $
      filter_ (\l -> l ^. bmlAction /=. val_ System) $
      orderedLogEntries migrateDb

collectAppliedMigrations :: BeamMigrateContext -> IO [MigrationName]
collectAppliedMigrations ctx = do
  BeamDatabaseRunner { bdbrBackend = be@BeamMigrationBackend {}
                     , bdbrRun = run, bdbrMigrateDb = migrateDb' } <- getRunner ctx
  let migrateDb = queryableMigrateDb migrateDb'
  dieOnDdlError ctx $ run $ listAllAppliedMigrations be migrateDb

newtype BeamMigrateT be m a
    = BeamMigrateT (ReaderT (BeamMigratePayload be m) m a)
      deriving newtype (Monad, Applicative, Functor)

instance MonadTrans (BeamMigrateT be) where
    lift x = BeamMigrateT (lift x)

instance MonadIO m => MonadIO (BeamMigrateT be m) where
    liftIO x = BeamMigrateT (liftIO x)

data BeamMigratePayload be m where
    BeamMigratePayload :: MonadIO m => BeamMigrationBackend be m -> BeamMigrateContext -> BeamMigratePayload be m

liftWithLower :: Monad m => ((forall a. BeamMigrateT be m a -> m a) ->  m a) -> BeamMigrateT be m a
liftWithLower go = BeamMigrateT $ do
                     x <- ask
                     lift (go (\(BeamMigrateT a) -> runReaderT a x))

reportSyntax :: Monad m => BeamSqlBackendSyntax be -> ReaderT (BeamMigratePayload be m) m ()
reportSyntax syntax = do
  BeamMigratePayload BeamMigrationBackend { backendRenderSyntax = render } ctx <- ask
  when (ctx ^. bmcDumpSql) $ do
    liftIO (hPutStrLn stderr (render syntax))

instance MonadBeam be m => MonadBeam be (BeamMigrateT be m) where
    runReturningMany syntax callback =
        BeamMigrateT $ do
          ctx <- ask
          reportSyntax syntax
          lift (runReturningMany syntax (\x -> let BeamMigrateT cb' = callback (BeamMigrateT (lift x))
                                               in runReaderT cb' ctx))

    runNoReturn syntax =
      BeamMigrateT $ do
        reportSyntax syntax
        lift (runNoReturn syntax)

    runReturningOne syntax =
      BeamMigrateT $ do
        reportSyntax syntax
        lift (runReturningOne syntax)

    runReturningFirst syntax =
      BeamMigrateT $ do
        reportSyntax syntax
        lift (runReturningFirst syntax)

    runReturningList syntax =
      BeamMigrateT $ do
        reportSyntax syntax
        lift (runReturningList syntax)

runMigration :: BeamMigrateContext -> (forall be m. (MonadBeam be m, MonadIO m, HasSqlEqualityCheck be T.Text) => BeamMigrationBackend be m -> DatabaseSettings be BeamMigrateDb -> BeamMigrateT be m a) -> IO (Either DdlError a)
runMigration ctx go = do
  BeamDatabaseRunner { bdbrBackend = be@BeamMigrationBackend {}
                     , bdbrRun = run, bdbrMigrateDb = migrateDb' } <- getRunner ctx
  run $
    let BeamMigrateT go' = go be (queryableMigrateDb migrateDb')
    in runReaderT go' (BeamMigratePayload be ctx)

readMigration :: BeamMigrateContext -> MigrateFile -> MigrationName -> IO Text
readMigration ctx fl nm = T.readFile (fullFilePath ctx (MigrateFile nm fl))

withBeamMigrationBackend :: BeamMigrateContext -> (forall be m. (MonadBeam be m, MonadIO m, HasSqlEqualityCheck be T.Text) => BeamMigrationBackend be m -> a) -> a
withBeamMigrationBackend BeamMigrateContext { _bmcRunner = Just BeamDatabaseRunner { bdbrBackend = be@BeamMigrationBackend {} } } go =
    go be
withBeamMigrationBackend ctx _ = noDatabaseError ctx

getCurrentUser :: BeamMigrateContext -> IO Text
getCurrentUser ctx =
  ctx ^. bmcOpts . beamMigrateGetCurrentUser

calcMigrationHash :: Text -> Text
calcMigrationHash t = TE.decodeUtf8 (convertToBase Base16 (hashWith SHA512 (TE.encodeUtf8 t)))

readCurrentDbPoint :: BeamMigrateContext -> IO DatabasePoint
readCurrentDbPoint ctx = readDbPoint ctx =<< getRunner ctx

readDbPoint :: BeamMigrateContext -> BeamDatabaseRunner -> IO DatabasePoint
readDbPoint ctx
            BeamDatabaseRunner { bdbrBackend = be@BeamMigrationBackend { backendGetDbConstraints = readSchema }
                                             :: BeamMigrationBackend be m
                               , bdbrRun = run } =
  dieOnDdlError ctx $ run $ flip runReaderT (BeamMigratePayload be ctx) $ do
    let isBeamMigratePredicate (SomeDatabasePredicate p) =
            any (\(SomeDatabasePredicate p') ->
                     SomeDatabasePredicate p == SomeDatabasePredicate p' ||
                     predicateCascadesDropOn p p'
                ) beamMigratePredicates
        beamMigratePredicates = collectChecks (beamMigrateDb @be)
    DatabasePoint . filter (not . isBeamMigratePredicate) <$> lift readSchema

data DbSourceEnum
    = DbSourceDatabase
      -- ^ The database connected to
    | DbSourceSchema
      -- ^ The expected schema of the database connected to
    | DbSourceMigration !MigrationName !RelTime
    | DbSourceBranch !BranchName
    | DbSourceConn !BeamDatabaseRunner
    | DbSourceEmpty

data RelTime = Before | After deriving (Show, Eq, Ord)

withDbSource :: BeamMigrateContext -> DbSource -> (DbSourceEnum -> IO a) -> IO a
withDbSource ctx src =
    bracket (parseDbSource ctx src) (closeDbSource ctx)

closeDbSource :: BeamMigrateContext -> DbSourceEnum -> IO ()
closeDbSource _ctx (DbSourceConn BeamDatabaseRunner { bdbrClose = close }) = close
closeDbSource _ _ = pure ()

parseDbSource :: BeamMigrateContext -> DbSource -> IO DbSourceEnum
parseDbSource ctx (DbSource src)
  | src == "0" = pure DbSourceEmpty
  | src == "SCHEMA" = pure DbSourceSchema
  | Just rem <- T.stripPrefix "DB" src =
      if T.null rem
      then pure DbSourceDatabase
      else
        case parseRel rem of
          Nothing -> invalidSource
          Just ct -> do
            mLog :: Maybe BeamMigrateLog <-
                dieOnDdlError ctx $ runMigration ctx $ \BeamMigrationBackend {} migrateDb ->
                    runSelectReturningFirst $ select $ offset_ (fromIntegral ct) $
                    orderBy_ (\l -> desc_ (l ^. bmlId)) $
                    filter_ (\l -> l ^. bmlAction /=. val_ System) $
                    all_ (migrateDb ^. bmdbLog)
            case mLog of
              Nothing -> invalidSource
              Just log -> pure (DbSourceMigration (log ^. bmlName) $
                                case log ^. bmlAction of
                                  Revert -> Before
                                  _ -> After)
  | Just rem <- T.stripPrefix "HEAD" src = do
      branch <- getCurrentBranchOrDie ctx
      reg <- readCurrentRegistry ctx
      case parseRel rem of
        Nothing -> invalidSource
        Just ct -> do
          mMigration <- getLatestCommittedMigration ctx branch
          case mMigration of
            Nothing -> invalidSource
            Just (nm, _) -> do
              mi' <- getParent ct nm
              pure (DbSourceMigration (mi' ^. miName) After)
  | Just rem <- T.stripPrefix "@" src = do
      mMig <- getLatestMigration ctx (BranchName rem)
      case mMig of
        Nothing -> invalidSource
        Just (_, _) -> pure (DbSourceBranch (BranchName rem))
  | otherwise = do
      -- If the name is a migration name, then continue
      reg <- readCurrentRegistry ctx
      case lookupMigration (MigrationName src) reg of
        Nothing -> do
          mMig <- getLatestMigration ctx (BranchName src)
          case mMig of
            Nothing -> DbSourceConn <$> openDatabase (T.unpack src)
            Just _ -> pure (DbSourceBranch (BranchName src))
        Just mi -> pure (DbSourceMigration (mi ^. miName) After)
  where
    invalidSource :: forall a. IO a
    invalidSource = beamMigrateError ctx ("Invalid database source: " <> text src)

    openDatabase nm = do
      mConn <- (ctx ^. bmcOpts . beamMigrateRunInDatabase) nm
      case mConn of
        Nothing -> case find (isBackendConnStr nm) (ctx ^. bmcOpts . beamMigrateBackends) of
                     Nothing -> invalidSource
                     Just (SomeCliMigrationBackend be@BeamMigrationBackend { backendConnect = connect }  db beamMigrateDb) -> do
                         let conn' = drop (length (backendName be) + 1) nm
                         BeamMigrateConnection { backendRun = run, backendClose = close } <- connect conn'
                         pure (BeamDatabaseRunner be db beamMigrateDb run close)
        Just db -> pure db

    isBackendConnStr :: String -> SomeCliMigrationBackend -> Bool
    isBackendConnStr conn (SomeCliMigrationBackend BeamMigrationBackend { backendName = nm } _ _) =
        (nm ++ ":") `isPrefixOf` conn

    parseRel rem = foldrM (\c n -> if c == '^' then pure (n + 1) else empty) 0 (T.unpack rem)
    getParent n nm = do
      reg <- readCurrentRegistry ctx
      case lookupMigration nm reg of
        Nothing -> invalidSource
        Just mi | n == 0 -> pure mi
                | otherwise ->
                    case mi ^. miDependencies of
                      [parent] -> getParent (n - 1) parent
                      [] -> invalidSource
                      _ -> beamMigrateError ctx ("Source " <> text src <> " is ambiguous at migration " <> pretty nm <> "^")

readDatabasePointFromSource :: BeamMigrateContext -> DbSourceEnum -> IO DatabasePoint
readDatabasePointFromSource ctx DbSourceDatabase = readCurrentDbPoint ctx
readDatabasePointFromSource ctx (DbSourceBranch nm) = do
  mMig <- getLatestMigration ctx nm
  case mMig of
    Nothing     -> beamMigrateError ctx ("No migration available for " <> pretty nm)
    Just (nm, _) -> readDatabasePointFromSource ctx (DbSourceMigration nm After)
readDatabasePointFromSource ctx (DbSourceMigration nm tm) = do
  sch <- readMigrationSchema ctx nm
  case tm of
    After -> pure (sch ^. msEndingSchema)
    Before -> pure (sch ^. msStartingSchema)
readDatabasePointFromSource ctx (DbSourceConn db) = readDbPoint ctx db
readDatabasePointFromSource _ DbSourceEmpty = pure (DatabasePoint mempty)
readDatabasePointFromSource ctx DbSourceSchema = do
  BeamDatabaseRunner { bdbrDatabase = schemaModel } <- getRunner ctx
  pure (DatabasePoint (collectChecks schemaModel))

getMigrationFromDbSource :: BeamMigrateContext -> DbSourceEnum -> IO (Maybe MigrationName)
getMigrationFromDbSource ctx (DbSourceBranch nm) = do
  preview (_Just . _1) <$> getLatestMigration ctx nm
getMigrationFromDbSource ctx (DbSourceMigration nm _) = pure (Just nm)
getMigrationFromDbSource _ _ = pure Nothing

getBackendFromDbSource :: BeamMigrateContext -> DbSourceEnum -> IO SomeBeamMigrationBackend
getBackendFromDbSource ctx (DbSourceConn BeamDatabaseRunner { bdbrBackend = be }) = pure (SomeBeamMigrationBackend be)
getBackendFromDbSource ctx _ = do
  BeamDatabaseRunner { bdbrBackend = be } <- getRunner ctx
  pure (SomeBeamMigrationBackend be)

dbSourceHasBackend :: BeamMigrateContext -> DbSourceEnum -> Bool
dbSourceHasBackend _ (DbSourceConn {}) = True
dbSourceHasBackend BeamMigrateContext { _bmcRunner = Just {} } _ = True
dbSourceHasBackend _ _ = False

-- * Diff

data DbDiff = DbDiff (HS.HashSet SomeDatabasePredicate) (HS.HashSet SomeDatabasePredicate)
              deriving Show

diffPoints :: DatabasePoint -> DatabasePoint -> DbDiff
diffPoints (DatabasePoint from) (DatabasePoint to) =
  let fromSet = HS.fromList from; toSet = HS.fromList to
      removed = fromSet `HS.difference` toSet
      added = toSet `HS.difference` fromSet
  in DbDiff removed added

noDifference :: DbDiff -> Bool
noDifference (DbDiff a b) = HS.null a && HS.null b

reportDatabaseDifference :: BeamMigrateContext -> DbDiff -> IO ()
reportDatabaseDifference ctx (DbDiff removed added) = beamMigrateMessage ctx (removedMsg <> extraLine <> addedMsg)
  where
    extraLine = if HS.null removed || HS.null added then mempty else line
    removedMsg =
        if HS.null removed then mempty
        else hang 2 ("The following predicates were removed:" <> line <>
                     vcat (map (\(SomeDatabasePredicate p) -> "-" <+> fromString (englishDescription p)) (HS.toList removed)))

    addedMsg =
        if HS.null added then mempty
        else hang 2 ("The following predicates were added:" <> line <>
                     vcat (map (\(SomeDatabasePredicate p) -> "-" <+> fromString (englishDescription p)) (HS.toList added)))
