{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Beam.Migrate.Cli.Registry where

import           Control.Exception (Exception, throwIO)
import           Control.Lens (makeLenses, (^..), each, (^.), makePrisms, (&), (%~))
import           Control.Monad (filterM)

import           Data.Char (isSpace, isAlphaNum)
import qualified Data.Graph.Inductive as Gr
import qualified Data.Graph.Inductive.PatriciaTree as Gr
import qualified Data.Map.Strict as M
import           Data.Maybe (mapMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Vector (Vector)
import qualified Data.Vector as V

import           System.Directory (doesDirectoryExist, getCurrentDirectory, doesPathExist)
import           System.FilePath (takeDirectory, (</>))
import           System.Environment (lookupEnv)

#ifdef mingw32_HOST_OS
import           System.Win32.File (getVolumeInformation) -- Windows-specific
#else
import           System.Posix.Files (getFileStatus, deviceID)
import           Control.Monad (forM)
#endif

-- * Find Registry

-- List of common version control directories
vcDirs :: [FilePath]
vcDirs = [ ".git", ".hg", ".svn", ".bzr", "CVS"
         , "_darcs", ".fossil-settings", ".p4", "_MTN", "BitKeeper" ]

#ifdef mingw32_HOST_OS
-- Windows: Get volume ID (equivalent of filesystem ID on POSIX)
getFsID :: FilePath -> IO (Maybe String)
getFsID path = do
    exists <- doesPathExist path
    if exists
        then do
            (_, _, _, volSerialNumber, _, _, _, _) <- getVolumeInformation path
            return (Just $ show volSerialNumber)
        else return Nothing
#else
-- POSIX: Get filesystem ID
getFsID :: FilePath -> IO (Maybe String)
getFsID path = do
    exists <- doesPathExist path
    if exists
      then do
        sts <- getFileStatus path
        pure (Just (show (deviceID sts)))
      else pure Nothing
#endif

-- | Recursively find a directory containing a version control system, avoiding FS boundaries unless allowed
findVCDir :: [FilePath] -> FilePath -> Maybe FilePath -> IO (Maybe FilePath)
findVCDir vcDirs dir mRootFs = do
    exists <- filterM (doesDirectoryExist . (dir </>)) vcDirs
    if not (null exists)
        then return (Just dir)
        else do
            let parent = takeDirectory dir
            if parent == dir  -- Stop at root directory
                then return Nothing
                else do
                    -- Check for FS boundary unless BEAM_MIGRATE_ACROSS_FILESYSTEMS is set
                    crossFsAllowed <- lookupEnv "BEAM_MIGRATE_ACROSS_FILESYSTEMS"
                    currentFs <- getFsID parent
                    if crossFsAllowed == Just "1" || (mRootFs == currentFs)
                        then findVCDir vcDirs parent mRootFs
                        else return Nothing

-- | Default mechanism to find a registry path. Search upwards for a git / mercurial / whatever repository
defaultFindRegistry :: IO FilePath
defaultFindRegistry = do
  mRegEnv <- lookupEnv "BEAM_MIGRATE_REGISTRY"
  case mRegEnv of
    Just regEnv -> pure regEnv
    Nothing -> do
      cwd <- getCurrentDirectory
      rootFs <- getFsID cwd
      beamRegistry <- findVCDir [".beam-migrate"] cwd rootFs
      case beamRegistry of
        Just bmRoot   -> pure (bmRoot </> ".beam-migrate")
        Nothing       -> do
          result <- findVCDir vcDirs cwd rootFs
          case result of
              Just vcRoot -> pure vcRoot
              Nothing     -> pure cwd

defaultFindMigrationsDir :: FilePath -> IO FilePath
defaultFindMigrationsDir registryFile =
    pure (takeDirectory registryFile </> "beam_migrations") -- TODO

-- * Registry declaration

data Registry
    = Registry
    { _regLines :: Vector RegEntry
    , _regGraph   :: Gr.Gr () ()
    } deriving Show

data RegEntry
    = RegEntryLine !Text
    | RegEntryMigration !MigrationInfo
      deriving Show

newtype MigrationName = MigrationName Text deriving (Show, Eq, Ord)

data MigrationInfo
    = MigrationInfo
    { _miName :: !MigrationName
    , _miCommitMessage :: !Text
    , _miFullText :: !Text
    , _miDependencies :: [MigrationName]
    , _miBranchStatus :: [BranchStatus]
    } deriving Show

data BranchStatus
    = BranchStatus CommitStatus BranchName
      deriving Show

newtype BranchName = BranchName Text
    deriving (Show, Eq)

data CommitStatus
    = Committed
      -- ^ This migration was committed to the database on this branch
    | Working
      -- ^ This migration is being worked on in this branch
      deriving Show

makeLenses ''Registry
makeLenses ''MigrationInfo
makePrisms ''RegEntry

defaultMigrationInfo :: MigrationInfo
defaultMigrationInfo = MigrationInfo { _miName = MigrationName ""
                                     , _miCommitMessage = ""
                                     , _miFullText = ""
                                     , _miDependencies = []
                                     , _miBranchStatus = [] }

newRegistry :: Registry
newRegistry = Registry { _regLines = mempty
                       , _regGraph = Gr.empty }

readRegistry :: FilePath -> IO Registry
readRegistry registryPath = do
  registryText <- T.readFile registryPath
  case readRegistryLines (T.lines registryText) of
    Nothing -> throwIO (RegIOError "Could not read registry")
    Just reg -> pure reg

data RegIOError = RegIOError String deriving (Show, Exception)

readRegistryLines :: [Text] -> Maybe Registry
readRegistryLines = go mempty []
  where
    go :: M.Map MigrationName Int -> [(RegEntry, Maybe (Gr.Context () ()))] -> [Text] -> Maybe Registry
    go _parsedNames revEntries [] =
        Just Registry { _regLines = V.reverse (V.fromList (fst <$> revEntries))
                      , _regGraph = Gr.buildGr (mapMaybe snd revEntries) }
    go parsedNames revEntries (line:lines)
        | "[" `T.isPrefixOf` line = do
            (entry, lines') <- parseEntry revEntries (line:lines)
            incomingDeps <- forM (_miDependencies entry) $ \nm -> do
                              n <- M.lookup nm parsedNames
                              pure ((), n)
            -- Lookup entries to make graph
            let ctxt = (incomingDeps, n, (), [])
                n = length revEntries
            n `seq` go (M.insert (_miName entry) n parsedNames) ((RegEntryMigration entry, Just ctxt):revEntries) lines'
        | otherwise = go parsedNames ((RegEntryLine line, Nothing):revEntries) lines

    parseEntry :: [(RegEntry, Maybe (Gr.Context () ()))] -> [Text] -> Maybe (MigrationInfo, [Text])
    parseEntry revEntries (line:lines) =
        case readDeps line of
          ([], _) -> Nothing -- error "Impossible"
          (name:deps, line') ->
              let (branchNames, line'') = readBranchNames line'
                  commitMessageFirst = parseFirstLine line''
                  (msgLines, fullText, lines') = readCommitMessage lines
                  msg = commitMessageFirst <> msgLines
              in Just ( MigrationInfo { _miName = name
                                      , _miCommitMessage = msg
                                      , _miFullText = T.unlines (line:fullText)
                                      , _miDependencies = deps
                                      , _miBranchStatus = branchNames }
                      , lines' )

    skipWhitespace = T.dropWhile isSpace
    readDeps :: Text -> ([MigrationName], Text)
    readDeps = readDeps' id

    readDeps' :: ([MigrationName]->[MigrationName])
              -> Text -> ([MigrationName], Text)
    readDeps' a t =
        case T.uncons t of
          Just ('[', t') ->
              let (migName, t'') = T.break (== ']') t'
              in case T.uncons t'' of
                   Just (']', t''') -> readDeps' (a . (MigrationName migName:)) (skipWhitespace t''')
                   _ -> (a [], t)
          Just ('|', t') -> (a [], t)
          _ -> (a [], t)

    readCommitMessage :: [Text] -> (Text, [Text], [Text])
    readCommitMessage ts = readCommitMessage' ts mempty id

    readCommitMessage' [] t a =
        (t, a [], [])
    readCommitMessage' (line:lines) t a
        | Just msgLine <- T.stripPrefix "    " line =
            readCommitMessage' lines (t <> msgLine) (a . (line:))
        | otherwise = (t, a [], line:lines)

    readBranchNames :: Text -> ([BranchStatus], Text)
    readBranchNames = readBranchNames' []

    readBranchNames' a t =
        case T.uncons t of
          Just (c, t')
            | Just commitStatus <- readCommitStatus c ->
              let (name, t'') = T.break isSpace t'
              in readBranchNames' (BranchStatus commitStatus (BranchName name):a) (skipWhitespace t'')
            | c == '|' -> (a, t)
          _ -> (a, t)

    readCommitStatus '@' = Just Committed
    readCommitStatus '~' = Just Working
    readCommitStatus _ = Nothing

    parseFirstLine line =
        let line' = skipWhitespace line
        in case T.uncons line' of
             Just ('|', t') -> skipWhitespace t'
             Nothing -> line'

isValidMigrationName :: MigrationName -> Bool
isValidMigrationName (MigrationName b) =
    case T.uncons b of
      Nothing -> False
      Just (start, b') -> T.all isValidMigrationChar b' && isValidMigrationStartChar start
   where
     isValidMigrationChar c = isAlphaNum c || c == '_' ||c == '-'
     isValidMigrationStartChar c = isAlphaNum c

findMigrationsForBranch :: Registry -> BranchName -> [(MigrationInfo, BranchStatus)]
findMigrationsForBranch reg branch =
    [ (mi, sts)
    | mi <- reg ^.. regLines . each . _RegEntryMigration
    , sts@(BranchStatus _ nm) <- mi ^. miBranchStatus
    , nm == branch ]

lookupMigration :: MigrationName -> Registry -> Maybe MigrationInfo
lookupMigration nm reg = do
  RegEntryMigration mi <- V.unsafeIndex (reg ^. regLines) <$> lookupMigrationIndex nm reg
  pure mi

lookupMigrationIndex :: MigrationName -> Registry -> Maybe Int
lookupMigrationIndex nm reg = V.findIndex nameMatches (reg ^. regLines)
  where
    nameMatches RegEntryLine {} = False
    nameMatches (RegEntryMigration mi) = mi ^. miName == nm

addRegistryEntry :: Registry -> RegEntry -> Either String Registry
addRegistryEntry reg e@(RegEntryLine {}) =
    Right (reg & regLines %~ (<> V.singleton e))
addRegistryEntry reg e@(RegEntryMigration mi) = do
  let n = V.length (reg ^. regLines)
  deps <- forM (mi ^. miDependencies) $ \dep ->
          case lookupMigrationIndex dep reg of
            Nothing -> Left ("Could not find migration " <> show dep)
            Just n -> pure n

  let incoming = map ((,) ()) deps
      context = (incoming, n, (), [])

  pure (reg & regLines %~ (<> V.singleton e)
            & regGraph %~ (context Gr.&))
