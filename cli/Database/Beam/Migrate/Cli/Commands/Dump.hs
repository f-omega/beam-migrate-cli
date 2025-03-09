{-# LANGUAGE OverloadedStrings #-}

module Database.Beam.Migrate.Cli.Commands.Dump where

import           Database.Beam.Migrate
import           Database.Beam.Migrate.Cli.Commands.Common
import           Database.Beam.Migrate.Cli.Message
import           Database.Beam.Migrate.Cli.Types

import           Control.Lens ((^.))
import           Control.Monad (when, forM_)

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as BL
import           Data.String (fromString)
import qualified Data.Text.Encoding as TE

beamMigrateDump :: BeamMigrateContext -> DumpCmd -> IO ()
beamMigrateDump ctx cmd = do
  let json :: JSON.ToJSON a => a -> IO ()
      json x = let j = BL.toStrict $ JSON.encode x
               in beamMigrateOutput ctx (text (TE.decodeUtf8 j))

  when (cmd ^. dumpJson) $ beamMigrateOutput ctx "{"
  forM_ (cmd ^. dumpSources) $ \dump@(DbSource nm) -> do
    withDbSource ctx dump $ \src -> do
      if cmd ^. dumpJson
         then json nm >> beamMigrateOutput ctx ": "
         else beamMigrateOutput ctx (text nm <> ":")

      pt@(DatabasePoint ps) <- readDatabasePointFromSource ctx src
      if cmd ^. dumpJson
         then json (serializeDatabasePoint ctx pt)
         else forM_ ps $ \(SomeDatabasePredicate p) ->
                beamMigrateOutput ctx ("  -" <+> fromString (englishDescription p))

      when (cmd ^. dumpJson) (beamMigrateOutput ctx ",")
