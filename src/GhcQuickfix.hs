{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module GhcQuickfix
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
import           Data.List (stripPrefix)
import           Data.Maybe
import           Data.Monoid (First(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import           Data.Traversable
import qualified DeferredFolds.UnfoldlM as DF
import qualified StmContainers.Map as SM
import qualified System.Directory as Dir
import qualified System.Environment as Env

import qualified GhcQuickfix.GhcFacade as Ghc

type ErrMap = SM.Map FilePath [T.Text]

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
    TIO.writeFile (fromMaybe "errors.err" mErrFilePath) $ T.unlines prunedMsgs
    threadDelay 200_000 -- 200ms

parseFilePathModifier :: [Ghc.CommandLineOption] -> Either String [T.Text -> T.Text]
parseFilePathModifier opts =
  case partitionEithers (mapMaybe getModifier opts) of
    ([], modifiers) -> Right modifiers
    (errs, _) -> Left (unlines errs)
  where
  getModifier opt = do
    pat <- stripPrefix "--quickfix-path-replace=" opt
    case T.split (== ':') (T.pack pat) of
      [needle, replace] -> Just . Right $ T.replace needle replace
      _ -> Just . Left $ "Malformed --quickfix-path-replace argument: expected format 'needle:replace', got '" ++ pat ++ "'"

parseQuickfixFilePath :: [Ghc.CommandLineOption] -> Maybe FilePath
parseQuickfixFilePath = getFirst . foldMap (First . stripPrefix "--quickfix-file=")

explicitlyEnabled :: [Ghc.CommandLineOption] -> IO Bool
explicitlyEnabled opts = do
  envEnabled <- (== Just "true") . fmap (map Char.toLower)
    <$> Env.lookupEnv "GHC_QUICKFIX_ENABLED"
  pure $ elem "--quickfix" opts || envEnabled

modifyHscEnv :: Bool -> [Ghc.CommandLineOption] -> Ghc.HscEnv -> IO Ghc.HscEnv
modifyHscEnv isOffByDefault opts hscEnv = do
  enabled <- explicitlyEnabled opts
  if not isOffByDefault || enabled then
    case parseFilePathModifier opts of
      Left err -> fail err
      Right filePathMods -> do
        errMap <- SM.newIO
        errsUpdated <- newTVarIO False
        void . Async.async $ writeQuickfixLoop (parseQuickfixFilePath opts) errMap errsUpdated
        pure hscEnv { Ghc.hsc_hooks = modifyHooks filePathMods (Ghc.hsc_hooks hscEnv) errMap errsUpdated }
  else
    pure hscEnv
  where
    modifyHooks filePathMods hooks (errMap :: ErrMap) (errsUpdated :: TVar Bool) =
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
                handleMessages filePathMods errMap errsUpdated msgs
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
                    else handleMessages filePathMods errMap errsUpdated $
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
    <> ":" <> T.show line <> ":" <> T.show col <> ": " <> severity <> ": " <> msg

-- | Update state given all diagnostics for a module
handleMessages :: [T.Text -> T.Text] -> ErrMap -> TVar Bool -> Ghc.Messages Ghc.GhcMessage -> IO ()
handleMessages filePathMods errMap errsUpdated messages = do
  let envelopes = Ghc.getMessages messages
      -- Filter out parse errors, HLint reports them already
      errs = mapMaybe (formatDiagnostic filePathMods)
           . filter (not . isParseError . Ghc.errMsgDiagnostic)
           $ Ghc.bagToList envelopes
      isParseError = \case
        Ghc.GhcPsMessage{} -> True
        _ -> False
      First mFile =
        foldMap
          (First . fmap Ghc.unpackFS . Ghc.srcSpanFileName_maybe . Ghc.errMsgSpan)
          $ Ghc.getMessages messages
  for_ mFile $ \file -> atomically $ do
    SM.insert errs file errMap
    writeTVar errsUpdated True

-- | Remove errors for files that no longer exist
pruneDeletedFiles :: [(FilePath, [T.Text])] -> ErrMap -> IO [T.Text]
pruneDeletedFiles errs errMap = do
  let files = fst <$> errs
  deletedFiles <- fmap catMaybes $
    for files $ \file ->
      Dir.doesFileExist file >>= \case
        True -> pure Nothing
        False -> pure (Just file)
  atomically $ traverse_ (`SM.delete` errMap) deletedFiles
  pure . foldMap snd $ filter (not . (`elem` deletedFiles) . fst) errs

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
            ghcMessage = Ghc.GhcDsMessage . Ghc.DsUnknownMessage $ Ghc.UnknownDiagnostic id diag
            warn = Ghc.mkMsgEnvelope diagOpts srcSpan Ghc.neverQualify ghcMessage
        modifyIORef dsWarnVar (Ghc.addMessage warn)
    _ -> pure ()
  logAction flags clss srcSpan sdoc
