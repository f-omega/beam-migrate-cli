module Database.Beam.Migrate.Cli.VC where

import           Database.Beam.Migrate.Cli.Engine.Internal
import           Database.Beam.Migrate.Cli.Registry
import           Database.Beam.Migrate.Cli.Types

import           Control.Exception (SomeException(..), catch)

import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as T

import           System.Exit (ExitCode(..))
import           System.Process

-- | Attempt to use a variety of version control systems to get the current branch.
--
-- We support a few of the most commonly used VC systems. If you need
-- a custom script, set the '_beamMigrateVcGetBranch' field of "BeamMigrateOptions".
defaultVcGetBranch :: BeamMigrateContext -> IO (Maybe BranchName)
defaultVcGetBranch _ = tryInOrder [ getGitBranch ]

getGitBranch :: IO (Maybe BranchName)
getGitBranch = do
  isGit <- isInsideGit
  if isGit
    then do
      (ec, out, _) <- readProcessWithExitCode "git" [ "rev-parse", "--abbrev-ref", "HEAD" ] ""
      case ec of
        ExitSuccess ->
            case filter (not . null) (lines out) of
              [nm] -> pure (Just (BranchName (T.strip (fromString nm))))
              _ -> pure Nothing
        _ -> pure Nothing
    else pure Nothing

isInsideGit :: IO Bool
isInsideGit = do
  (ec, _, _) <- readProcessWithExitCode "git" [ "rev-parse", "--is-inside-work-tree" ] ""
  case ec of
    ExitSuccess -> pure True
    _ -> pure False

tryInOrder :: [ IO (Maybe a) ] -> IO (Maybe a)
tryInOrder [] = pure Nothing
tryInOrder (x:xs) = do
  mx <- x `catch` (\SomeException {} -> pure Nothing)
  case mx of
    Nothing -> tryInOrder xs
    Just r -> pure (Just r)
