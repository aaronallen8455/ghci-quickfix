{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module GhciQuickfix
  ( plugin
  , pluginOffByDefault
  ) where

import           Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.STM.TVar
import           Control.Exception
import qualified Control.Foldl as F
import           Control.Monad
import           Control.Monad.STM
import qualified Data.Char as Char
import           Data.Either (partitionEithers)
import           Data.Foldable
import           Data.IORef
import qualified Data.List as List
import           Data.Maybe
import           Data.Monoid (First(..))
import qualified Data.Ord as Ord
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import           Data.Traversable
import qualified DeferredFolds.UnfoldlM as DF
import qualified StmContainers.Map as SM
import qualified System.Directory as Dir
import qualified System.Environment as Env

import qualified GhciQuickfix.GhcFacade as Ghc

-- | STM Map is used to avoid contention between modules compiled in parrallel,
-- although it isn't clear if it actually helps since contention will still
-- happen with the STVars used in the same transaction.
-- The Int paired with the errors is used for ordering.
type ErrMap = SM.Map FilePath (Int, [T.Text])

plugin :: Ghc.Plugin
plugin = Ghc.defaultPlugin
  { Ghc.driverPlugin = modifyHscEnv False
  , Ghc.pluginRecompile = mempty
  }

-- | For use with repl-alliance, where this plugin should be off by default
pluginOffByDefault :: Ghc.Plugin
pluginOffByDefault = plugin
  { Ghc.driverPlugin = modifyHscEnv True }

-- | Background process that writes the quickfix file when errors change. Adds a
-- delay to mitigate excessive IO.
writeQuickfixLoop :: Maybe FilePath -> ErrMap -> TVar Bool -> IO ()
writeQuickfixLoop mErrFilePath errMap updated = forever $ do
    msgs <- atomically $ do
      check =<< readTVar updated
      writeTVar updated False
      DF.foldM (F.generalize F.list) (SM.unfoldlM errMap)
    prunedMsgs <- pruneDeletedFiles msgs errMap
    TIO.writeFile (fromMaybe "errors.err" mErrFilePath)
      . T.unlines . foldMap snd
      -- Sort in reverse so that the first error in the list is the last one to
      -- be emitted by the compiler
      $ List.sortOn Ord.Down prunedMsgs
    threadDelay 200_000 -- 200ms

parseFilePathModifier :: [Ghc.CommandLineOption] -> IO (Either String [T.Text -> T.Text])
parseFilePathModifier opts = do
  envMod <- getEnvModifier
  pure $ case partitionEithers (mapMaybe getModifier opts ++ maybeToList envMod) of
    ([], modifiers) -> Right modifiers
    (errs, _) -> Left (unlines errs)
  where
  parseReplacement :: String -> String -> Either String (T.Text -> T.Text)
  parseReplacement source pat =
    case T.split (== ':') (T.pack pat) of
      [needle, replace] -> Right $ T.replace needle replace
      _ -> Left $ "Malformed " ++ source ++ ": expected format 'needle:replace', got '" ++ pat ++ "'"
  getEnvModifier = do
    mPat <- Env.lookupEnv "GHCI_QUICKFIX_PATH_REPLACE"
    pure $ parseReplacement "GHCI_QUICKFIX_PATH_REPLACE environment variable" <$> mPat
  getModifier opt = do
    pat <- List.stripPrefix "--quickfix-path-replace=" opt
    Just $ parseReplacement "--quickfix-path-replace argument" pat

parseQuickfixFilePath :: [Ghc.CommandLineOption] -> IO (Maybe FilePath)
parseQuickfixFilePath opts = do
  envPath <- Env.lookupEnv "GHCI_QUICKFIX_FILE"
  pure $ getFirst $ foldMap (First . List.stripPrefix "--quickfix-file=") opts <> First envPath

parseIncludeParserErrors :: [Ghc.CommandLineOption] -> IO Bool
parseIncludeParserErrors opts = do
  envEnabled <- (== Just "true") . fmap (map Char.toLower)
    <$> Env.lookupEnv "GHCI_QUICKFIX_INCLUDE_PARSER_ERRORS"
  pure $ elem "--quickfix-include-parser-errors" opts || envEnabled

explicitlyEnabled :: [Ghc.CommandLineOption] -> IO Bool
explicitlyEnabled opts = do
  envEnabled <- (== Just "true") . fmap (map Char.toLower)
    <$> Env.lookupEnv "GHCI_QUICKFIX_ENABLED"
  pure $ elem "--quickfix" opts || envEnabled

modifyHscEnv :: Bool -> [Ghc.CommandLineOption] -> Ghc.HscEnv -> IO Ghc.HscEnv
modifyHscEnv isOffByDefault opts hscEnv = do
  enabled <- explicitlyEnabled opts
  if not isOffByDefault || enabled then do
    parseFilePathModifier opts >>= \case
      Left err -> fail err
      Right filePathMods -> do
        errMap <- SM.newIO
        errsUpdated <- newTVarIO False
        counter <- newTVarIO 0
        quickfixFilePath <- parseQuickfixFilePath opts
        void . Async.async $ writeQuickfixLoop quickfixFilePath errMap errsUpdated
        includeParserErrors <- parseIncludeParserErrors opts
        pure hscEnv { Ghc.hsc_hooks = modifyHooks includeParserErrors filePathMods (Ghc.hsc_hooks hscEnv) errMap errsUpdated counter }
  else
    pure hscEnv
  where
    modifyHooks includeParserErrors filePathMods hooks (errMap :: ErrMap) (errsUpdated :: TVar Bool) (counter :: TVar Int) =
      let runPhaseOrExistingHook :: Ghc.TPhase res -> IO res
          runPhaseOrExistingHook = maybe Ghc.runPhase (\(Ghc.PhaseHook h) -> h)
            $ Ghc.runPhaseHook hooks
          phaseHook :: Ghc.PhaseHook
          phaseHook = Ghc.PhaseHook $ \phase -> do
            let tcWarnings :: Ghc.Messages Ghc.GhcMessage
                tcWarnings = case phase of
                  Ghc.T_HscPostTc _ _ _ msgs _ -> msgs
                  _ -> mempty
            dsWarnVar <- newIORef mempty
            try (runPhaseOrExistingHook $ addDsLogHook (logHookHack dsWarnVar hscEnv) phase) >>= \case
              Left err@(Ghc.SourceError msgs) -> do
                handleMessages includeParserErrors filePathMods errMap errsUpdated counter msgs
                throw err
              Right res -> do
                dsWarns <- readIORef dsWarnVar
                case phase of
                  Ghc.T_HscPostTc _ modSummary _ _ _ ->
                    if Ghc.isEmptyMessages dsWarns
                    then atomically $ do
                      -- Module compiled without errors or warnings so delete map entry if exists
                      let modFile = Ghc.ms_hspp_file modSummary
                      SM.lookup modFile errMap >>= \case
                        Nothing -> pure ()
                        Just _ -> do
                          SM.delete (Ghc.ms_hspp_file modSummary) errMap
                          writeTVar errsUpdated True
                    else handleMessages includeParserErrors filePathMods errMap errsUpdated counter $
                      if length tcWarnings == length dsWarns
                      then tcWarnings -- has preferred formatting
                      else dsWarns
                  _ -> pure ()
                pure res
       in hooks
            { Ghc.runPhaseHook = Just phaseHook }

addDsLogHook :: (Ghc.LogAction -> Ghc.LogAction) -> Ghc.TPhase res -> Ghc.TPhase res
addDsLogHook logHook = \case
  Ghc.T_HscPostTc hscEnv a b c d ->
    Ghc.T_HscPostTc (addHook hscEnv) a b c d
  x -> x
  where
    addHook hscEnv = hscEnv { Ghc.hsc_logger = Ghc.pushLogHook logHook $ Ghc.hsc_logger hscEnv }

-- | Get a textual representation of the diagnostic in GCC format
formatDiagnostic :: [T.Text -> T.Text] -> Ghc.MsgEnvelope Ghc.GhcMessage -> Maybe T.Text
formatDiagnostic filePathMods m = do
  severity <- case Ghc.errMsgSeverity m of
    Ghc.SevIgnore -> Nothing
    -- ^ Ignore this message, for example in case of suppression of warnings
    -- users don't want to see.
    Ghc.SevWarning -> Just "warning"
    Ghc.SevError -> Just "error"
  startLoc <- Ghc.realSrcSpanStart <$> Ghc.srcSpanToRealSrcSpan (Ghc.errMsgSpan m)
  let diag = Ghc.errMsgDiagnostic m
      opts = (Ghc.defaultDiagnosticOpts @Ghc.GhcMessage)
        { Ghc.tcMessageOpts = (Ghc.defaultDiagnosticOpts @Ghc.TcRnMessage)
          { Ghc.tcOptsShowContext = False -- Omit all the additional stuff
          }
        }
      ctx = Ghc.defaultSDocContext
        { Ghc.sdocStyle = Ghc.mkErrStyle (Ghc.errMsgContext m)
        , Ghc.sdocCanUseUnicode = True
        }

      file = TE.decodeUtf8 . Ghc.bytesFS $ Ghc.srcLocFile startLoc
      line = Ghc.srcLocLine startLoc
      col = Ghc.srcLocCol startLoc
      truncateMsg txt =
        let truncated = T.take 200 txt
        in if T.length txt > 200 then truncated <> "…" else truncated
      msg = truncateMsg . T.intercalate " • "
        $ T.unwords . T.words . T.pack
        . Ghc.renderWithContext ctx
        <$> filter (not . Ghc.isEmpty ctx) (Ghc.unDecorated (Ghc.diagnosticMessage opts diag))

  -- filename:line:column: error: message
  Just $ foldl' (flip ($)) file filePathMods
    <> ":" <> T.pack (show line) <> ":" <> T.pack (show col) <> ": "
    <> severity <> ": " <> msg

-- | Update state given all diagnostics for a module
handleMessages
  :: Bool
  -> [T.Text -> T.Text]
  -> ErrMap
  -> TVar Bool
  -> TVar Int
  -> Ghc.Messages Ghc.GhcMessage
  -> IO ()
handleMessages includeParserErrors filePathMods errMap errsUpdated counter messages = do
  let envelopes = Ghc.getMessages messages
      isParseError = \case
        Ghc.GhcPsMessage{} -> True
        _ -> False
      -- Filter out parse errors unless explicitly included
      errs = mapMaybe (formatDiagnostic filePathMods)
           . filter (\env -> includeParserErrors || not (isParseError (Ghc.errMsgDiagnostic env)))
           $ Ghc.bagToList envelopes
      First mFile =
        foldMap
          (First . fmap Ghc.unpackFS . Ghc.srcSpanFileName_maybe . Ghc.errMsgSpan)
          $ Ghc.getMessages messages
  for_ mFile $ \file -> atomically $ do
    n <- stateTVar counter (\x -> let !nx = x + 1 in (x, nx))
    SM.insert (n, errs) file errMap
    writeTVar errsUpdated True

-- | Remove errors for files that no longer exist
pruneDeletedFiles :: [(FilePath, (Int, [T.Text]))] -> ErrMap -> IO [(Int, [T.Text])]
pruneDeletedFiles errs errMap = do
  let files = fst <$> errs
  deletedFiles <- fmap catMaybes $
    for files $ \file ->
      Dir.doesFileExist file >>= \case
        True -> pure Nothing
        False -> pure (Just file)
  atomically $ traverse_ (`SM.delete` errMap) deletedFiles
  pure . fmap snd $ filter (not . (`elem` deletedFiles) . fst) errs

-- | Currently no good way to get warnings from desugarer, so a log action hook
-- is used to get the raw SDoc. Note: unfortunately this will also capture
-- warnings from the typechecker.
logHookHack :: IORef (Ghc.Messages Ghc.GhcMessage) -> Ghc.HscEnv -> Ghc.LogAction -> Ghc.LogAction
logHookHack dsWarnVar hscEnv logAction flags clss srcSpan sdoc = do
  case clss of
    Ghc.MCDiagnostic Ghc.SevWarning _ _ -> do
        let diag =
              Ghc.DiagnosticMessage
                { Ghc.diagMessage = Ghc.mkSimpleDecorated sdoc
                , Ghc.diagReason = Ghc.WarningWithoutFlag
                , Ghc.diagHints = []
                }
            diagOpts = Ghc.initDiagOpts $ Ghc.hsc_dflags hscEnv
            ghcMessage = Ghc.GhcDsMessage . Ghc.DsUnknownMessage $ Ghc.mkUnknownDiagnostic diag
            warn = Ghc.mkMsgEnvelope diagOpts srcSpan Ghc.neverQualify ghcMessage
        modifyIORef dsWarnVar (Ghc.addMessage warn)
    _ -> pure ()
  logAction flags clss srcSpan sdoc
