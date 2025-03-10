module Database.Beam.Migrate.Cli.Engine
    ( Pickle, PickledMigration
    , newPickle, registerApplyScript, registerRevertScript, addMigrationEdge

    , bringUpToDate, bringUpToDate', defaultBeamMigrateDb
    ) where

import           Database.Beam
import           Database.Beam.Migrate (HasDefaultSqlDataType, unCheckDatabase)
import           Database.Beam.Migrate.Backend
import           Database.Beam.Migrate.Cli.Engine.Database (MigrationName(..), BeamMigrateLogT(..), BeamMigrateAction(..), BeamMigrateDb, migrateTableExistsPredicate, getLastBeamMigrateVersion', insertBeamMigrationVersion, listAllAppliedMigrations, beamMigrateDb, BeamMigrateCliBackend, calcMigrationHash, bmdbLog, getNextEntryId)
import           Database.Beam.Migrate.Cli.Engine.Database.Migrations (getMigrationsFrom)
import           Database.Beam.Migrate.Cli.Engine.Migrate
    (dominatorGraph, dualGraph, findPathMaybe, findLastCommonDominator, migrate, findTips, MigrationAction(..))

import           Control.Lens ((^.))
import           Control.Monad (unless)

import           Data.Foldable (forM_)
import qualified Data.Graph.Inductive as Gr
import           Data.List (sort, nub, find)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Ord (comparing)
import           Data.String (fromString)
import           Data.Text (Text)
import           Data.Time (LocalTime, getZonedTime, zonedTimeToLocalTime)
import qualified Data.Vector as V


-- | A set of pickled migrations that can be embedded into a binary
data Pickle
    = Pickle
    { pickleMigrations :: [PickledMigration]
    -- ^ Names of migrations
    , pickleDeps :: [(MigrationName, MigrationName)]
    -- ^ Registered dependencies
    , pickleTip :: MigrationName
    -- ^ The name of the migration that is the 'tip'/latest.
    } deriving Show

data PickledMigration
    = PickledMigration
    { pmName     :: !MigrationName
    , pmContents :: !Text
    , pmRevert   :: !Bool
    } deriving Show

{-# INLINE newPickle #-}
newPickle :: String -> Pickle
newPickle tip = Pickle { pickleMigrations = []
                       , pickleDeps = []
                       , pickleTip = MigrationName (fromString tip) }

{-# INLINE registerApplyScript #-}
registerApplyScript :: String -> String -> Pickle -> Pickle
registerApplyScript nm contents pickle =
    let p = PickledMigration { pmName = MigrationName $ fromString nm
                             , pmContents = fromString contents
                             , pmRevert = False }
        mig' = p:pickleMigrations pickle
    in mig' `seq` pickle { pickleMigrations = mig' }

{-# INLINE registerRevertScript #-}
registerRevertScript :: String -> String -> Pickle -> Pickle
registerRevertScript nm contents pickle =
    let p = PickledMigration { pmName = MigrationName $ fromString nm
                             , pmContents = fromString contents
                             , pmRevert = True }
        mig' = p:pickleMigrations pickle
    in mig' `seq` pickle { pickleMigrations = mig' }

{-# INLINE addMigrationEdge #-}
addMigrationEdge :: String -> String -> Pickle -> Pickle
addMigrationEdge from to p =
    deps' `seq` p { pickleDeps = deps' }
  where
    deps' = (MigrationName (fromString from), MigrationName (fromString to)):pickleDeps p

graphFromPickle :: Pickle -> (Int, Gr.Gr (Maybe MigrationName) (), Int -> MigrationName, MigrationName -> Int)
graphFromPickle pickle =
    let names = zip [1..] (map Just sorted)
        sorted = nub (sort (pmName <$> pickleMigrations pickle))
        sortedVector = V.fromList sorted

        nameMap = Map.fromList (zip sorted [1..])
        ix a = fromMaybe (error "impossible") (Map.lookup a nameMap)
        edges = map (\(a, b) -> (ix a, ix b, ())) (pickleDeps pickle)

        gr = Gr.mkGraph ((0, Nothing):names) edges
        gr' = foldr (\n -> Gr.insEdge (0, n, ())) gr [n | n <- Gr.nodes gr, Gr.indeg gr n == 0, n /= 0 ]
    in (ix (pickleTip pickle), gr', ((V.!) sortedVector) . pred, ix)

defaultBeamMigrateDb :: BeamMigrateCliBackend be
                     => DatabaseSettings be BeamMigrateDb
defaultBeamMigrateDb = unCheckDatabase beamMigrateDb

bringUpToDate :: (BeamMigrateCliBackend be, MonadIO m, HasSqlEqualityCheck be Text, HasDefaultSqlDataType be LocalTime)
              => BeamMigrationBackend be m -> Pickle -> m ()
bringUpToDate be pk = bringUpToDate' be defaultBeamMigrateDb pk

bringUpToDate' :: (MonadIO m, HasSqlEqualityCheck be Text, HasDefaultSqlDataType be LocalTime)
               => BeamMigrationBackend be m -> DatabaseSettings be BeamMigrateDb -> Pickle -> m ()
bringUpToDate' be@BeamMigrationBackend { backendRunSqlScript = runSql
                                      , backendWithTransaction = tx
                                      , backendRenderSyntax = render
                                      , backendGetDbConstraints = getConstraints }
              migrateDb pickle = do
  cs <- getConstraints
  let tblExists = migrateTableExistsPredicate be migrateDb `elem` cs

  version <- if not tblExists
             then pure 0
             else do
               mVersion <- getLastBeamMigrateVersion' be migrateDb
               case mVersion of
                 Nothing -> error "Database is corrupted"
                 Just version -> pure version

  let dbMigrations = getMigrationsFrom be migrateDb version
  forM_ dbMigrations $ \(version, syntax) ->
    tx $ do
      let sql = fromString (unlines (render <$> syntax))
      runSql sql
      runInsert =<< insertBeamMigrationVersion be migrateDb mempty version

  -- Get tips from the database
  let (latest, gr, ixToMigration, migrationToIx) = graphFromPickle pickle

  allMigrations <- fmap migrationToIx <$> listAllAppliedMigrations be migrateDb
  let tips = findTips gr allMigrations
      insertEntry ty migration = do
        entryId <- getNextEntryId migrateDb
        today <- zonedTimeToLocalTime <$> liftIO getZonedTime
        runInsert $ insert (migrateDb ^. bmdbLog)
                  $ insertExpressions
                    [ BeamMigrateLog { _bmlId = val_ entryId
                                     , _bmlName = val_ (pmName migration)
                                     , _bmlBranch = nothing_
                                     , _bmlAction = val_ ty
                                     , _bmlUser = val_ mempty
                                     , _bmlDate = val_ today
                                     , _bmlNote = val_ mempty
                                     , _bmlMigrateHash = just_ (val_ (calcMigrationHash (pmContents migration)))
                                     , _bmlMigrateVersion = val_ version } ]

  -- Now the schema is up-to-date, apply the migrations
  case migrate gr tips latest of
    Left _err -> error ("bringUpToDate: could not apply database migrations: " ++ show _err ++ ": " ++ show gr)
    Right acts ->
      forM_ acts $ \act ->
        case act of
          ApplyPatch i  ->
            let name = ixToMigration i
            in case find (\pm -> pmName pm == name && not (pmRevert pm)) (pickleMigrations pickle) of
                 Nothing -> error "bringUpToDate: could not find apply script"
                 Just pm -> tx (runSql (pmContents pm) >> insertEntry Apply pm)
          RevertPatch i ->
            let name = ixToMigration i
            in case find (\pm -> pmName pm == name && pmRevert pm) (pickleMigrations pickle) of
                 Nothing -> error "bringUpToDate: could not find revert script"
                 Just pm -> tx (runSql (pmContents pm) >> insertEntry Revert pm)
