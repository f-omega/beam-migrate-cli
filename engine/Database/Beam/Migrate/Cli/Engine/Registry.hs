{-# LANGUAGE UndecidableInstances #-}

module Database.Beam.Migrate.Cli.Engine.Registry where

import           Database.Beam ( FromBackendRow, HasSqlEqualityCheck )
import           Database.Beam.Backend
    ( FromBackendRow,
      BeamSqlBackend,
      FromBackendRowM(FromBackendRowM),
      HasSqlValueSyntax,
      BeamBackend )

import           Database.Beam.Migrate ( HasDefaultSqlDataType )
import           Database.Beam.Migrate.Cli.Engine.Internal (BranchName, MigrationName)

import           Control.Lens (Lens', makeLenses, makePrisms, (^.), (^?), ix, (^..), each)

import qualified Data.Graph.Inductive as Gr
import qualified Data.Graph.Inductive.PatriciaTree as Gr
import qualified Data.Graph.Inductive.Query.Dominators as Gr
import qualified Data.Graph.Inductive.Query.SP as Gr
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import           Data.Traversable (forM)
import           Data.Vector (Vector)
import qualified Data.Vector as V

-- * Registry declaration

data Registry
    = Registry
    { _regLines :: Vector RegEntry
    , _regGraph :: Gr.Gr () ()
    } deriving Show


data RegEntry
    = RegEntryLine !Text
    | RegEntryMigration !MigrationInfo
    | RegEntryDeleted
    | RegEntryRoot
      deriving Show

data MigrationInfo
    = MigrationInfo
    { _miName :: !MigrationName
    , _miCommitMessage :: !Text
    , _miFullText :: [Text]
    , _miDependencies :: [MigrationName]
    , _miBranchStatus :: [BranchStatus]
    } deriving Show

data BranchStatus
    = BranchStatus CommitStatus BranchName
      deriving Show

branchStatusBranch :: Lens' BranchStatus BranchName
branchStatusBranch f (BranchStatus c n) = BranchStatus c <$> f n

branchStatusStatus :: Lens' BranchStatus CommitStatus
branchStatusStatus f (BranchStatus c n) = (\c' -> BranchStatus c' n) <$> f c

data CommitStatus
    = Committed
      -- ^ This migration was committed to the database on this branch
    | Working
      -- ^ This migration is being worked on in this branch
      deriving Show

makeLenses ''Registry
makeLenses ''MigrationInfo
makePrisms ''RegEntry
makePrisms ''CommitStatus

findMigrationsForBranch :: Registry -> BranchName -> [(MigrationInfo, BranchStatus)]
findMigrationsForBranch reg branch =
    [ (mi, sts)
    | mi <- reg ^.. regLines . each . _RegEntryMigration
    , sts@(BranchStatus _ nm) <- mi ^. miBranchStatus
    , nm == branch ]

lookupMigrationIndex :: MigrationName -> Registry -> Maybe Int
lookupMigrationIndex nm reg = V.findIndex nameMatches (reg ^. regLines)
  where
    nameMatches RegEntryLine {} = False
    nameMatches (RegEntryMigration mi) = mi ^. miName == nm
    nameMatches RegEntryRoot = False

lookupMigrationIndexNoFail :: MigrationName -> Registry -> Int
lookupMigrationIndexNoFail nm reg =
  fromMaybe (error "Could not find migration") (lookupMigrationIndex nm reg)

-- | Given a list of migrations applied in a database, find all 'tips'
-- (i.e., those that have not been superseded).
migrationTips :: Registry -> [MigrationName] -> [MigrationName]
migrationTips reg migrations =
    let migrationIxs = map (flip lookupMigrationIndexNoFail reg) migrations
        subgraph = Gr.nfilter (\n -> n `elem` migrationIxs) (reg ^. regGraph)
        tips = map (\(i, _) -> (reg ^. regLines) `V.unsafeIndex` i) $
               filter (\(n, _) -> Gr.outdeg subgraph n == 0) (Gr.labNodes subgraph)
    in tips ^.. each . _RegEntryMigration . miName

dominatorForTips :: Registry -> [MigrationName] -> Maybe MigrationName
dominatorForTips reg migrations =
    let migrationIxs = map (flip lookupMigrationIndexNoFail reg) migrations

        domGraph = regDomGraph reg

        paths = map (\to -> fromMaybe (error "Internal") (Gr.sp 0 to domGraph)) migrationIxs

        lca answer paths =
          case paths of
            (a:t):nexts'
               | all matches nexts' ->
                   lca (succ answer) (t:map tail nexts')
               where matches (a':_) = a == a'
                     matches _ = False
            _ -> answer

        domIx = lca 0 paths
    in reg ^? regLines . ix domIx . _RegEntryMigration . miName

regDomGraph :: Registry -> Gr.Gr () Int
regDomGraph reg = Gr.mkGraph (Gr.labNodes (reg ^. regGraph)) (map (\(t, f) -> (f, t, 1)) idoms)
    where
      idoms = Gr.iDom (reg ^. regGraph) 0

migrationsFor :: Registry -> Maybe MigrationName -> Maybe MigrationName -> Maybe [MigrationInfo]
migrationsFor reg from to =
    let fromIx = maybe 0 (flip lookupMigrationIndexNoFail reg) from
        toIx = maybe 0 (flip lookupMigrationIndexNoFail reg) to
        doms = regDomGraph reg
    in do
      lines <- map (V.unsafeIndex (reg ^. regLines)) <$> Gr.sp fromIx toIx doms
      case lines of
        [] -> pure []
        _:lines' -> do
           entries <- forM lines' $ \l -> do
                      RegEntryMigration mi <- pure l
                      pure mi
           pure entries
