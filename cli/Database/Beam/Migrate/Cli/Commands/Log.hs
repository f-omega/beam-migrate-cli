{-# LANGUAGE OverloadedStrings #-}

module Database.Beam.Migrate.Cli.Commands.Log where

import           Database.Beam
import           Database.Beam.Migrate.Backend
import           Database.Beam.Migrate.Cli.Commands.Common
import           Database.Beam.Migrate.Cli.Database
import           Database.Beam.Migrate.Cli.Message
import           Database.Beam.Migrate.Cli.Registry
import           Database.Beam.Migrate.Cli.Types hiding (CliCommand(..))

import           Control.Lens ((^.))
import           Control.Monad (forM_)

import qualified Data.Text as T

beamMigrateLog :: BeamMigrateContext -> LogCmd -> IO ()
beamMigrateLog ctx cmd = do
  log :: [BeamMigrateLog] <-
    dieOnDdlError ctx $ runMigration ctx $ \BeamMigrationBackend {} migrateDb ->
           runSelectReturningList $ select $
           orderBy_ (\l -> desc_ (l ^. bmlId)) $
           filter_ (\l -> l ^. bmlAction /=. val_ System) $
           all_ (migrateDb ^. bmdbLog)

  reg <- readCurrentRegistry ctx
  forM_ log $ \log -> do
    beamMigrateOutput ctx (prettyLog log (lookupMigration (log ^. bmlName) reg))

prettyLog :: BeamMigrateLog -> Maybe MigrationInfo -> Message
prettyLog l mMigration =
    hang 4 (vcat [ brackets action <+> pretty (l ^. bmlName) <+> bolden firstLogLine
--                 , "Hash: " <+> text (l ^. bmlMigrateHash)
                 , bolden "Date:" <+> unAnnotate (pretty (l ^. bmlDate))
                 , bolden "User:" <+> text (l ^. bmlUser) ] <> remainingLogLines)
  where
    action = case l ^. bmlAction of
               Apply -> dullGreen "apply"
               Revert -> dullRed "revert"
               System -> blue "system"

    remainingLogLines =
        hashLine <>
        case remainingLogLines' of
          [] -> mempty
          ls -> line <> vcat ls

    hashLine = case l ^. bmlMigrateHash of
                 Nothing -> mempty
                 Just hash -> line <> bolden "Hash:" <+> text hash

    (firstLogLine, remainingLogLines')
        | T.null (l ^. bmlNote) = (firstMsgLine, remMsgLines)
        | otherwise = (text $ l ^. bmlNote, firstMsgLine:remMsgLines)

    (firstMsgLine, remMsgLines) =
        case mMigration of
          Nothing -> (mempty, mempty)
          Just mi ->
              case T.lines (mi ^. miCommitMessage) of
                [] -> (mempty, mempty)
                (x:xs) -> (text x, map text xs)
