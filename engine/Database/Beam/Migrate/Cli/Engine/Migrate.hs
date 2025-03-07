module Database.Beam.Migrate.Cli.Engine.Migrate where

import           Data.Foldable (elem)
import qualified Data.Graph.Inductive as Gr
import qualified Data.Graph.Inductive.Query.Dominators as Gr
import           Data.List (find)
import qualified Data.List.NonEmpty as NEL
import           Data.Maybe (fromMaybe)
import           Data.Monoid (Ap(Ap, getAp))

dominatorGraph :: forall gr' gr n e. (Gr.Graph gr, Gr.Graph gr') => gr n e -> gr' n ()
dominatorGraph gr = Gr.mkGraph (Gr.labNodes gr) (map (\(t, f) -> (f, t, ())) idoms)
    where
      idoms = Gr.iDom gr 0

dualGraph :: forall gr' gr n e. (Gr.Graph gr, Gr.Graph gr') => gr n e -> gr' n e
dualGraph gr = Gr.mkGraph (Gr.labNodes gr) (map (\(t, f, e) -> (f, t, e)) (Gr.labEdges gr))

findPathMaybe :: Gr.DynGraph gr => gr n e -> Int -> Int -> Maybe [Int]
findPathMaybe gr from to =
    let labeled = Gr.emap (const 1) gr
    in Gr.sp from to labeled

findPath :: Gr.DynGraph gr => gr n e -> Int -> Int -> [Int]
findPath gr from to = fromMaybe (error "findPath: no path") (findPathMaybe gr from to)

findLastCommonDominator :: Gr.DynGraph gr => gr n e -> Int -> [Int] -> Maybe Int
findLastCommonDominator gr root tips = do
  reversedPaths <- fmap reverse <$> mapM (findPathMaybe gr root) tips

  let go [] = Nothing
      go xs = do
          case find (isCommonAncestor xs . NEL.head) xs of
            Nothing -> go =<< mapM (snd . NEL.uncons) xs
            Just (x NEL.:| _) -> pure x

      isCommonAncestor xss tip =
          all (tip `elem`) xss

  go =<< mapM NEL.nonEmpty reversedPaths

data MigrationAction
    = RevertPatch !Int
    | ApplyPatch !Int
      deriving Show

data MigrationError = NoLastCommonDominator
                    | NoPathFound Int Int
                      deriving Show

migrate :: forall gr node edge
         . Gr.DynGraph gr => gr node edge -> [Int] -> Int -> Either MigrationError [MigrationAction]
migrate graph tips latest =
    let domGr = dominatorGraph @gr graph
        reversed = dualGraph @gr domGr

        mDom = findLastCommonDominator domGr 0 tips
    in case mDom of
         Nothing  -> Left NoLastCommonDominator
         Just dom ->
           let path = maybe (Left (NoPathFound dom latest)) Right $
                      (map ApplyPatch <$> findPathMaybe domGr dom latest)

               migrationToDom tip = maybe (Left (NoPathFound tip dom)) Right $
                                    (map RevertPatch <$> findPathMaybe reversed tip dom)
           in (<>)
              -- Revert all tips to d, in some order (shouldn't matter)
              <$> getAp (foldMap (Ap . migrationToDom) tips)
              -- Then migrate from dominator to the current latest version
              <*> path

findTips :: Gr.DynGraph gr => gr node edge -> [Int] -> [Int]
findTips gr migIxs =
    let subgraph = Gr.nfilter (\n -> any (`elem` migIxs) (Gr.dfs [n] gr)) gr
        tips = filter (\n -> Gr.outdeg subgraph n == 0) (Gr.nodes subgraph)
    in tips
