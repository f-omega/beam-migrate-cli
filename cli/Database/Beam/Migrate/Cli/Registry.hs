{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Beam.Migrate.Cli.Registry where

import           Database.Beam
import           Database.Beam.Backend (HasSqlValueSyntax, BeamBackend)
import           Database.Beam.Backend.SQL (BeamSqlBackend)
import           Database.Beam.Migrate (HasDefaultSqlDataType)
import           Database.Beam.Migrate.Cli.Engine.Internal (BranchName(..), MigrationName(..), findTips, findLastCommonDominator)

import           Control.Exception (Exception, throwIO, throw)
import           Control.Lens (makeLenses, (^..), each, (^.), makePrisms, (&), (%~), to, (.~), Lens', ix, (^?), toListOf)
import           Control.Monad (filterM)

import           Data.Char (isSpace, isAlphaNum)
import qualified Data.Graph.Inductive as Gr
import qualified Data.Graph.Inductive.Query.Dominators as Gr
import qualified Data.Graph.Inductive.Query.SP as Gr
import qualified Data.Graph.Inductive.PatriciaTree as Gr
import qualified Data.Map.Strict as M
import           Data.Maybe (mapMaybe, fromMaybe, maybeToList)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Vector (Vector)
import qualified Data.Vector as V

import           Debug.Trace (trace)

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
              Just vcRoot -> pure (vcRoot </> ".beam-migrate")
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
    | RegEntryDeleted
    | RegEntryRoot
      deriving Show

data MigrationInfo
    = MigrationInfo
    { _miName :: !MigrationName
    , _miCommitMessage :: !Text
    , _miFullText :: [Text]
    , _miDependencies :: [MigrationName]
    , _miBranchStatus :: [BranchStatus]
    } deriving Show

data BranchStatus
    = BranchStatus CommitStatus BranchName
      deriving Show

branchStatusBranch :: Lens' BranchStatus BranchName
branchStatusBranch f (BranchStatus c n) = BranchStatus c <$> f n

branchStatusStatus :: Lens' BranchStatus CommitStatus
branchStatusStatus f (BranchStatus c n) = (\c' -> BranchStatus c' n) <$> f c

updateOrCreateBranchStatus :: MigrationInfo -> BranchStatus -> MigrationInfo
updateOrCreateBranchStatus miInfo sts =
    let branch = sts ^. branchStatusBranch
        (found, stss') =
            foldr (\x (found, stss) ->
                       if branch == x ^. branchStatusBranch
                       then (True, sts:stss)
                       else (found, x:stss))
                  (False, []) (_miBranchStatus miInfo)
        stss'' | found = stss'
               | otherwise = sts:stss'
    in miInfo { _miBranchStatus = stss'' }

data CommitStatus
    = Committed
      -- ^ This migration was committed to the database on this branch
    | Working
      -- ^ This migration is being worked on in this branch
      deriving Show

makeLenses ''Registry
makeLenses ''MigrationInfo
makePrisms ''RegEntry
makePrisms ''CommitStatus

defaultMigrationInfo :: MigrationInfo
defaultMigrationInfo = MigrationInfo { _miName = MigrationName ""
                                     , _miCommitMessage = ""
                                     , _miFullText = []
                                     , _miDependencies = []
                                     , _miBranchStatus = [] }

newRegistry :: Registry
newRegistry = Registry { _regLines = mempty
                       , _regGraph = Gr.empty }

writeRegistry :: FilePath -> Registry -> IO ()
writeRegistry registryPath reg =
    T.writeFile registryPath (T.unlines (reg ^. regLines . each . to getLine))
  where
    getLine (RegEntryLine x) = [x]
    getLine RegEntryDeleted = []
    getLine RegEntryRoot = []
    getLine (RegEntryMigration mi) = mi ^. miFullText

readRegistry :: FilePath -> IO Registry
readRegistry registryPath = do
  registryText <- T.readFile registryPath
  case readRegistryLines (T.lines registryText) of
    Nothing -> throwIO (RegIOError "Could not read registry")
    Just reg -> pure reg

data RegIOError = RegIOError String deriving (Show, Exception)

readRegistryLines :: [Text] -> Maybe Registry
readRegistryLines = go mempty [(RegEntryRoot, Just ([], 0, (), []))]
  where
    go :: M.Map MigrationName Int -> [(RegEntry, Maybe (Gr.Context () ()))] -> [Text] -> Maybe Registry
    go _parsedNames revEntries [] =
        Just Registry { _regLines = V.reverse (V.fromList (fmap fst revEntries))
                      , _regGraph = Gr.buildGr (mapMaybe snd revEntries) }
    go parsedNames revEntries (line:lines)
        | "[" `T.isPrefixOf` line = do
            (entry, lines') <- parseEntry revEntries (line:lines)
            incomingDeps <- forM (_miDependencies entry) $ \nm -> do
                              n <- M.lookup nm parsedNames
                              pure ((), n)
            -- Lookup entries to make graph
            let ctxt = (incomingDeps', n, (), [])
                n = length revEntries
                incomingDeps' = case incomingDeps of
                                  [] -> [((), 0)]
                                  _ -> incomingDeps
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
                                      , _miFullText = line:fullText
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
             _ -> line'

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

removeMigrationFromRegistry :: MigrationName -> Registry -> Registry
removeMigrationFromRegistry nm reg =
    case lookupMigrationIndex nm reg of
      Nothing -> reg
      Just  i -> reg & regLines . ix i .~ RegEntryDeleted

lookupMigration :: MigrationName -> Registry -> Maybe MigrationInfo
lookupMigration nm reg = do
  RegEntryMigration mi <- V.unsafeIndex (reg ^. regLines) <$> lookupMigrationIndex nm reg
  pure mi

lookupMigrationIndex :: MigrationName -> Registry -> Maybe Int
lookupMigrationIndex nm reg = V.findIndex nameMatches (reg ^. regLines)
  where
    nameMatches RegEntryLine {} = False
    nameMatches (RegEntryMigration mi) = mi ^. miName == nm
    nameMatches RegEntryRoot = False

lookupMigrationIndexNoFail :: MigrationName -> Registry -> Int
lookupMigrationIndexNoFail nm reg =
  fromMaybe (error "Could not find migration") (lookupMigrationIndex nm reg)

addRegistryEntry :: Registry -> RegEntry -> Either String Registry
addRegistryEntry reg e@(RegEntryLine {}) =
    Right (reg & regLines %~ (<> V.singleton e))
addRegistryEntry reg e@(RegEntryMigration orig) = do
  case lookupMigrationIndex (orig ^. miName) reg of
    Just {} -> Left ("Migration " <> show (orig ^. miName) <> " already exists")
    _ -> pure ()

  let n = V.length (reg ^. regLines)
      mi = formatMigrationInfo orig
  deps <- forM (mi ^. miDependencies) $ \dep ->
          case lookupMigrationIndex dep reg of
            Nothing -> Left ("Could not find migration " <> show dep)
            Just n -> pure n

  let incoming = map ((,) ()) deps
      context = (incoming, n, (), [])

  pure (reg & regLines %~ (<> V.singleton (RegEntryMigration mi))
            & regGraph %~ (context Gr.&))

updateMigrationInRegistry :: MigrationInfo -> Registry -> Registry
updateMigrationInRegistry mi = updateMigrationInRegistryByName (mi ^. miName) (\_ -> mi)

updateMigrationInRegistryByName :: MigrationName -> (MigrationInfo -> MigrationInfo) -> Registry -> Registry
updateMigrationInRegistryByName nm mod reg =
    case lookupMigrationIndex nm reg of
      Nothing -> error "updateMigrationInRegistry: not found"
      Just  i -> reg & regLines . ix i . _RegEntryMigration %~ formatMigrationInfo . mod

-- | Update the miFullText field based on the info
formatMigrationInfo :: MigrationInfo -> MigrationInfo
formatMigrationInfo mi =
    mi & miFullText .~ lines
  where
    (msgFirstLine, msgLines) =
        case T.lines (mi ^. miCommitMessage) of
          [] -> ("", [])
          l:ls -> (l, ls)

    lines = firstLine:fmap formatExtraCommitLines msgLines

    formatExtraCommitLines = ("    " <>)

    skipWhitespace = T.dropWhile isSpace
    formatFirstLine firstLine =
        let firstLine' = skipWhitespace firstLine
        in case T.uncons firstLine' of
             Just (c, _) | isSpecial c -> " | " <> firstLine'
             _ -> " " <> firstLine'

    isSpecial '[' = True
    isSpecial ']' = True
    isSpecial '|' = True
    isSpecial '@' = True
    isSpecial '~' = True
    isSpecial _ = False

    formatMigrationName (MigrationName nm) = "[" <> nm <> "]"

    formatBranchStatus (BranchStatus Working (BranchName nm)) = "~" <> nm
    formatBranchStatus (BranchStatus Committed (BranchName nm)) = "@" <> nm

    firstLine = T.unwords (map formatMigrationName (mi ^. miName:mi ^. miDependencies) <>
                           map formatBranchStatus (mi ^. miBranchStatus)) <>
                formatFirstLine msgFirstLine

-- | Given a list of migrations applied in a database, find all 'tips'
-- (i.e., those that have not been superseded).
migrationTips :: Registry -> [MigrationName] -> [MigrationName]
migrationTips reg migrations =
    let tips = map (V.unsafeIndex (reg ^. regLines)) (findTips (reg ^. regGraph) migrationIxs)
        migrationIxs = map (flip lookupMigrationIndexNoFail reg) migrations
--        subgraph = Gr.nfilter (\n -> n `elem` migrationIxs) (reg ^. regGraph)
--        tips = map (\(i, _) -> (reg ^. regLines) `V.unsafeIndex` i) $
--               filter (\(n, _) -> Gr.outdeg subgraph n == 0) (Gr.labNodes subgraph)
    in tips ^.. each . _RegEntryMigration . miName

dominatorForTips :: Registry -> [MigrationName] -> Maybe MigrationName
dominatorForTips reg migrations = do
    let migrationIxs = map (flip lookupMigrationIndexNoFail reg) migrations

        domGraph = regDomGraph reg

    domIx <- findLastCommonDominator domGraph 0 migrationIxs
    reg ^? regLines . ix domIx . _RegEntryMigration . miName

regDomGraph :: Registry -> Gr.Gr () Int
regDomGraph reg = Gr.mkGraph (Gr.labNodes (reg ^. regGraph)) (map (\(t, f) -> (f, t, 1)) idoms)
    where
      idoms = Gr.iDom (reg ^. regGraph) 0

migrationsFor :: Registry -> Maybe MigrationName -> Maybe MigrationName -> Maybe [MigrationInfo]
migrationsFor reg from to =
    let fromIx = maybe 0 (flip lookupMigrationIndexNoFail reg) from
        toIx = maybe 0 (flip lookupMigrationIndexNoFail reg) to
        doms = regDomGraph reg
    in do
      lines <- map (V.unsafeIndex (reg ^. regLines)) <$> Gr.sp fromIx toIx doms
      case lines of
        [] -> pure []
        _:lines' -> do
           entries <- forM lines' $ \l -> do
                      RegEntryMigration mi <- pure l
                      pure mi
           pure entries

registryBranches :: Registry -> [BranchName]
registryBranches = toListOf (regLines . each . _RegEntryMigration . miBranchStatus . each . branchStatusBranch)

mkMigrationClosure :: Registry -> [MigrationName] -> ([MigrationName], [(MigrationName, MigrationName)])
mkMigrationClosure reg nms =
    let ixs = map (flip lookupMigrationIndexNoFail reg) nms
        closureIxs = Gr.rdfs ixs (reg ^. regGraph)
        migNames = toListOf (each . _RegEntryMigration . miName)
                     (map (V.unsafeIndex (reg ^. regLines)) closureIxs)

        edges = do
          (a, b) <- Gr.edges (Gr.nfilter (`elem` closureIxs) (reg ^. regGraph))
          aNm <- maybeToList (V.unsafeIndex (reg ^. regLines) a ^? _RegEntryMigration . miName)
          bNm <- maybeToList (V.unsafeIndex (reg ^. regLines) b ^? _RegEntryMigration . miName)
          pure (aNm, bNm)
    in (migNames, edges)
