{-# LANGUAGE OverloadedStrings #-}

module Database.Beam.Migrate.Cli.Commands.Status where

import           Database.Beam.Migrate.Cli.Commands.Common
import           Database.Beam.Migrate.Cli.Commands.ManageDb (showDatabaseStatus)
import           Database.Beam.Migrate.Cli.Message
import           Database.Beam.Migrate.Cli.Registry
import           Database.Beam.Migrate.Cli.Types

import           Control.Lens ((^.))
import           Control.Monad (when)

beamMigrateStatus :: BeamMigrateContext -> StatusCmd -> IO ()
beamMigrateStatus ctx cmd = do
  let statusLine = beamMigrateMessage ctx . info

  registre <- readCurrentRegistry ctx

  mBranch <- getCurrentBranch ctx
  case mBranch of
    Nothing -> beamMigrateMessage ctx (errorMessage "Could not determine current branch")
    Just branch -> do
      let formatBranchMessage fromBranch
              | fromBranch == branch = "."
              | otherwise = "," <> indent 2 (line <> "which is from branch " <> pretty fromBranch)

      statusLine ("On branch " <> pretty branch)

      mMigration <- getLatestMigration ctx branch
      case mMigration of
        Nothing -> statusLine (indent 2 (warning "No migration registered for this branch." <> line <> line <>
                                         "Run " <> cmdline ctx "add" <> " to add an initial migration"))
        Just (migration, BranchStatus sts fromBranch) -> do
            let branchMessage = formatBranchMessage fromBranch
            case sts of
              Committed ->
                  statusLine (indent 2 ("Latest migration is " <> pretty migration <> branchMessage))
              Working -> do
                mCommitted <- getLatestCommittedMigration ctx branch
                case mCommitted of
                  Nothing ->
                    statusLine (indent 2 ("There is an unstaged initial migration " <> pretty migration <> branchMessage <>
                                          line <> "Run " <> cmdline ctx "commit" <> " to commit this migration."))
                  Just (committedMigration, BranchStatus _ committedFromBranch) ->
                    statusLine (indent 2 ("There is an unstaged migration " <> pretty migration <> branchMessage <>
                                          line <> "Run " <> cmdline ctx "commit" <> " to commit this migration." <>
                                          line <> line <>
                                          "Latest committed migration is " <> pretty committedMigration <> formatBranchMessage committedFromBranch))

  statusLine mempty
  showDatabaseStatus ctx (cmd ^. statusForceDbStatus)
