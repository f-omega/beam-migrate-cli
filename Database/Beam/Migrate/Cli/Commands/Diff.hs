{-# LANGUAGE OverloadedStrings #-}

module Database.Beam.Migrate.Cli.Commands.Diff where

import           Database.Beam.Backend.SQL
import           Database.Beam.Haskell.Syntax
import           Database.Beam.Migrate
import           Database.Beam.Migrate.Actions
import           Database.Beam.Migrate.Backend
import           Database.Beam.Migrate.Cli.Commands.Common
import           Database.Beam.Migrate.Cli.Message
import           Database.Beam.Migrate.Cli.Types

import           Control.Lens ((^.))

import           Data.Foldable (toList)
import qualified Data.HashSet as HS
import           Data.Maybe (mapMaybe)
import           Data.Proxy (Proxy(..))
import           Data.String (fromString)
import           Data.Typeable ((:~:), eqT)

data DiffFormat where
    DiffFormat :: ([BeamSqlBackendSyntax be] -> Message)
               -> ActionProvider be -> DiffFormat

formatFromBackend :: SomeBeamMigrationBackend -> DiffFormat
formatFromBackend (SomeBeamMigrationBackend
                   BeamMigrationBackend { backendRenderSyntax = render
                                        , backendActionProvider = actions })
  = DiffFormat (vcat . map (fromString . render)) actions

isSameBackend :: BeamMigrationBackend be m -> BeamMigrationBackend be' m' -> Maybe (be :~: be')
isSameBackend BeamMigrationBackend {} BeamMigrationBackend {} = eqT

scrubDbSpecific :: [SomeDatabasePredicate] -> [SomeDatabasePredicate]
scrubDbSpecific = filter (not . isBackendSpecific)
  where
    isBackendSpecific (SomeDatabasePredicate (p :: p)) =
        case predicateSpecificity (Proxy @p) of
          PredicateSpecificityAllBackends -> False
          _ -> True

beamMigrateDiff :: BeamMigrateContext -> DiffCmd -> IO ()
beamMigrateDiff ctx cmd =
  withDbSource ctx (cmd ^. diffSource) $ \src ->
  withDbSource ctx (cmd ^. diffDest) $ \dest -> do
    let chosenFormat =
            case cmd ^. diffFormat of
              -- Prefer destination format, but only if it has a backend
              SomeFormat | dbSourceHasBackend ctx dest -> DestinationFormat
                         | dbSourceHasBackend ctx src  -> SourceFormat
                         | otherwise -> beamMigrateError ctx ("Neither the source nor destination database source is associated with a particular format.")
              x -> x

        inFormat source (DatabasePoint ss) =
            case chosenFormat of
              HaskellFormat
                | null ss -> pure (DatabasePoint ss)
                | otherwise -> do
                    SomeBeamMigrationBackend be@BeamMigrationBackend { backendConvertToHaskell = HaskellPredicateConverter convert } <- getBackendFromDbSource ctx source
                    pure (DatabasePoint (mapMaybe convert ss))
              SourceFormat
                | dbSourceHasBackend ctx source -> do
                    SomeBeamMigrationBackend be <- getBackendFromDbSource ctx source
                    SomeBeamMigrationBackend srcBackend <- getBackendFromDbSource ctx src
                    case isSameBackend be srcBackend of
                      Nothing -> pure (DatabasePoint (scrubDbSpecific ss))
                      Just  _ -> pure (DatabasePoint ss)
              DestinationFormat
                  | dbSourceHasBackend ctx source -> do
                      SomeBeamMigrationBackend be <- getBackendFromDbSource ctx source
                      SomeBeamMigrationBackend destBackend <- getBackendFromDbSource ctx dest
                      case isSameBackend be destBackend of
                        Nothing -> pure (DatabasePoint (scrubDbSpecific ss))
                        Just  _ -> pure (DatabasePoint ss)
              _ -> pure (DatabasePoint (scrubDbSpecific ss))

    DatabasePoint srcPt <- inFormat src =<< readDatabasePointFromSource ctx src
    DatabasePoint destPt <- inFormat dest =<< readDatabasePointFromSource ctx dest

    DiffFormat renderSrc solver <-
        case chosenFormat of
          HaskellFormat -> pure (DiffFormat @HsMigrateBackend
                                            (either (\_ -> error "could not render schema") fromString .
                                             renderHsSchema . hsActionsToModule "Db")
                                            defaultActionProvider)
          SourceFormat  -> formatFromBackend <$> getBackendFromDbSource ctx src
          DestinationFormat -> formatFromBackend <$> getBackendFromDbSource ctx dest
          SomeFormat -> error "impossible"

    whenVerbose ctx $
      beamMigrateMessage ctx (hang 2 ("Dest state:" <> line <> foldMap (\(SomeDatabasePredicate p) -> "- " <> fromString (englishDescription p) <> line) destPt))

    solution <- interactiveSolution ctx renderSrc (heuristicSolver solver srcPt destPt)
    case solution of
      Candidates [] -> beamMigrateError ctx "No migration found"
      Candidates (x:_) ->
          let missing = HS.fromList destPt `HS.difference` dbStateKey x
          in beamMigrateError ctx (hang 2 ("No migration found:" <> line <> foldMap (\(SomeDatabasePredicate p) -> "- " <> fromString (englishDescription p) <> line) missing) <> line <> "For migration: " <> line <>
                                  renderSrc (map migrationCommand (toList (dbStateCmdSequence x))))
      Solved cmds -> beamMigrateOutput ctx (renderSrc (map migrationCommand cmds))

interactiveSolution :: BeamMigrateContext -> ([BeamSqlBackendSyntax be] -> Message) -> Solver be -> IO (FinalSolution be)
interactiveSolution ctx renderSrc solver =
  case solver of
    ProvideSolution cmds -> pure (Solved cmds)
    SearchFailed xs -> pure (Candidates xs)
    ChooseActions { choosingActionsAtState = DatabaseState { dbStateKey = ss }
                  , getPotentialActionChoice = get
                  , potentialActionChoices = choices
                  , continueSearch = go } -> do
      whenVerbose ctx $
        beamMigrateMessage ctx ("---" <> line <>
                                hang 2 ("At state:" <> line <> printState ss) <> line <> line <>
                                hang 2 ("Actions are:" <> line <> foldMap (\a -> printAction (get a) <> line) choices))
      interactiveSolution ctx renderSrc (go choices)
  where
    printState = foldMap (\(SomeDatabasePredicate p) -> "- " <> fromString (englishDescription p) <> line)
    printAction PotentialAction { actionCommands = cmds
                                , actionEnglish = eng } =
      hang 2 ("- " <> text eng <> line <> renderSrc (fmap migrationCommand (toList cmds)))
