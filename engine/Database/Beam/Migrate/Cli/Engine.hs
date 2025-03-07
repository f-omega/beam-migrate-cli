module Database.Beam.Migrate.Cli.Engine where

import qualified Data.Graph.Inductive as Gr
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Vector as V

-- | A set of pickled migrations that can be embedded into a binary
data Pickle
    = Pickle
    { pickleMigrations :: [PickledMigration]
    -- ^ Names of migrations
    , pickleDeps :: [(Text, Text)]
    -- ^ Registered dependencies
    , pickleTip :: Text
    -- ^ The name of the migration that is the 'tip'/latest.
    } deriving Show

data PickledMigration
    = PickledMigration
    { pmName     :: !MigrationName
    , pmContents :: !Text
    } deriving Show

graphFromPickle :: Pickle -> (Int, Gr.Gr PickledMigration (), Int -> PickledMigration)
graphFromPickle pickle =
    let names = zip [0..] sorted
        sorted = sortBy (comparing . pmName) (pickleMigrations pickle)
        sortedVector = V.fromList sorted

        nameMap = Map.fromList (map swap names)
        ix a = fromMaybe (error "impossible") (Map.lookup a nameMap)
        edges = map (\(a, b) -> (ix a, ix b, ())) (pickleDeps pickle)

        gr = Gr.mkGraph names edges
    in (ix (pickleTip pickle), gr, (V.!) sortedVector)

data MigrationAction node
    = Revert !Int node
    | Apply !Int node
      deriving Show

migrate :: Gr.Gr node edge -> [Int] -> Int -> [MigrationAction node]
migrate graph tips latest =
    let domGr = dominatorGraph graph
        reversed = dualGraph domGr

        dom = findCommonDominator domGr tips
        path = map (uncurry Apply) (findPath domGr dom latest)

        migrationToDom tip = map (uncurry Revert) (findPath reversed tip dom)
    in case dom of
         Nothing -> error "migrate: impossible"
         Just  d ->
           -- Revert all tips to d, in some order (shouldn't matter)
           (foldMap migrationToDom tips) <>

           -- Then migrate from dominator to the current latest version
           path
