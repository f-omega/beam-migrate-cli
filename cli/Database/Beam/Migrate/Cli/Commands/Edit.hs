{-# LANGUAGE OverloadedStrings #-}

module Database.Beam.Migrate.Cli.Commands.Edit where

import Database.Beam.Migrate.Cli.Commands.Common
import Database.Beam.Migrate.Cli.Registry (lookupMigration)
import Database.Beam.Migrate.Cli.Types
import Database.Beam.Migrate.Cli.Message

import Control.Lens ((^.))

import System.Process (spawnProcess, waitForProcess)

import Text.Editor (runUserEditorDWIM, userEditorDefault, _default_editor)

beamMigrateEdit :: BeamMigrateContext -> EditCmd -> IO ()
beamMigrateEdit ctx cmd = do
  reg <- readCurrentRegistry ctx
  case lookupMigration (cmd ^. editMigration) reg of
    Nothing -> beamMigrateError ctx ("Migration " <> pretty (cmd ^. editMigration) <> " does not exist")
    Just  _ -> do
      let flPath = MigrateFile (cmd ^. editMigration) (cmd ^. editFile)
          path = fullFilePath ctx flPath
      editor <- userEditorDefault _default_editor
      phdl <- spawnProcess editor [path]
      waitForProcess phdl
      pure ()
