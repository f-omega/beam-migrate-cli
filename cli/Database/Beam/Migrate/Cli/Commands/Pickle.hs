{-# LANGUAGE OverloadedStrings #-}

module Database.Beam.Migrate.Cli.Commands.Pickle where

import           Database.Beam.Migrate.Cli.Commands.Common
import           Database.Beam.Migrate.Cli.Engine.Internal
import           Database.Beam.Migrate.Cli.Message
import           Database.Beam.Migrate.Cli.Registry
import           Database.Beam.Migrate.Cli.Types

import           Control.Lens ((^.))

import           Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe)
import           Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Traversable (forM)

import           Debug.Trace
import Data.Graph.Inductive (dfs)

beamMigratePickle :: BeamMigrateContext -> PickleCmd -> IO ()
beamMigratePickle ctx cmd = do
  reg <- readCurrentRegistry ctx
  (branches, selective) <-
      case cmd ^. pickleBranches of
        SelectAllBranches -> pure (registryBranches reg, False)
        SelectNamedBranches bs -> pure (bs, True)
        SelectDefaultBranch -> (,True) . pure <$> getCurrentBranchOrDie ctx

  tipBranch <-
    case cmd ^. pickleTip of
      Nothing -> case branches of
                   [b] -> pure b
                   _ -> beamMigrateError ctx "Multiple branch names given, use the --tip option to determine which is used as the latest"
      Just t | t `elem` branches -> pure t
             | otherwise -> beamMigrateError ctx ("Invalid --tip: " <> pretty t <> " is not a valid branch")

  let getMigration =
          case cmd ^. pickleBranchStatus of
            IncludeWorking -> getLatestMigration ctx
            _ -> getLatestCommittedMigration ctx

  -- For each branch, get the latest migration. Error out if there's a working migration
  migrations <-
    fmap mconcat $
    forM branches $ \branch -> do
       mMig <- getMigration branch
       case mMig of
         Nothing
             | selective, CommittedOnly <- cmd ^. pickleBranchStatus
                -> beamMigrateError ctx ("Branch " <> pretty branch <> " has no migrations")
             | otherwise -> pure []
         Just (mig, _) -> pure [mig]

  -- compute all the paths in the dom graph from the root to this migration
  let (migrationClosure, migrationEdges) = mkMigrationClosure reg migrations

  obfuscateMigrationName <-
      if not (cmd ^. pickleObfuscate)
      then pure id
      else do
        migMap <-
            fmap M.fromList $
            forM migrationClosure $ \mig -> do
              hash <- calcMigrationHash <$> T.readFile (fullFilePath ctx (MigrateFile mig ApplyScript))
              pure (mig, MigrationName hash)

        pure (\x -> case M.lookup x migMap of
                      Nothing -> error "impossible"
                      Just nm -> nm)

  (obfuscateMigrationName -> MigrationName tip, _) <- fromMaybe (beamMigrateError ctx ("Branch " <> pretty tipBranch <> " has no migrations")) <$> getMigration tipBranch

  let files = do
        mig <- migrationClosure
        script <- [ ApplyScript, RevertScript ]
        pure (MigrateFile mig script)

      obfuscateFile (MigrateFile mig script) = pure (MigrateFile (obfuscateMigrationName mig) script)
      obfuscateFile x = pure x

      obfuscatedEdges = map (\(a, b) -> (obfuscateMigrationName a, obfuscateMigrationName b)) migrationEdges

  -- TODO only include revert scripts if needed
  pickled <- mapM (\fl -> do
                     contents <- T.readFile . fullFilePath ctx $ fl
                     fl' <- obfuscateFile fl
                     pure (fl', contents)) files

  let registerMigrationSnippet (fl, txt) =
          case fl of
            MigrateFile (MigrationName mig) ApplyScript ->
                hang 2 ("registerApplyScript" <+> fromString (show (T.unpack mig)) <> softline <> fromString (show (T.unpack txt)) <+> "$" <> softline)
            MigrateFile (MigrationName mig) RevertScript ->
                hang 2 ("registerRevertScript" <+> fromString (show (T.unpack mig)) <> softline <> fromString (show (T.unpack txt)) <+> "$" <> softline)
            _ -> mempty

      addEdge (MigrationName a, MigrationName b) = "addMigrationEdge" <+> fromString (show (T.unpack a)) <+> fromString (show (T.unpack b)) <+> "$" <> softline

  beamMigrateOutput ctx
    (mconcat $
     [ "module ", text (cmd ^. pickleModuleName), "(migrations) where", line, line
     , "import Database.Beam.Migrate.Cli.Engine", line, line
     , "migrations :: Pickle", line
     , hang 2 . mconcat $
       [ "migrations =", line
       , foldMap addEdge obfuscatedEdges
       , foldMap registerMigrationSnippet pickled
       , "newPickle" <+> fromString (show tip) ]])

