{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lib
    ( defaultMain
    ) where

-- TODO Split out?
import           Compton.Parser       (parseComptonFile)
import qualified Compton.Static       as CS
import           Compton.Types        (Comparer (..), Entry, OpacityValue (..),
                                       Selector (..), Value (..),
                                       getOpacityArray)
import           Compton.Utilities    (changeOpaciteeeh, flipEnabledBool,
                                       setEnabledBool, unsetEnabledBool)
import           Compton.Writer       (writeComptonConfig)
import           Control.Monad.Reader (MonadIO, MonadReader, ReaderT, ask,
                                       liftIO, runReaderT)
import           Data.List         (find)
import           Data.Text         (unpack)
import           Data.Text.IO      (readFile)
import qualified Frontend.DMenu    as Dmenu
import qualified Frontend.Terminal as Term
import           Prelude           hiding (flip, readFile)
import           Processes         (callXDOTool, callXProps, getComptonPID,
                                    kill, launchCompton, sendSIGUSR1)
import           Terminal.Parser      as TP
import           TypeMap              (nameMatchComparer,
                                       windowIdentifierSelector)
import           Wizard            (WizardState (..), wizardStep)
import           Xprops.Parser        (parseXpropOutput)
import           Xprops.Utilities     (getClassLine, getNameLine,
                                       windowIdentifierGetter)

newtype ConsReadT a = ConsReadT
  { runConsArgRead :: ReaderT ConsoleArguments IO a
  } deriving (Applicative, Monad, Functor, MonadReader ConsoleArguments, MonadIO)

data Perform
  = ConfigUpdate ([Entry] -> [Entry])
  | PrintList [String]
  | Kill
  -- TODO You're asking to be hurt
  | Restart (IO ())
  | RunWizard [Entry]

-- TODO Find something existing or generalize this
replaceOrAdd :: Entry -> [Entry] -> [Entry]
replaceOrAdd (name, value) [] = [(name, value)]
replaceOrAdd (name, value) ((curName, curValue):rest) =
  if curName == name
  then (name, value):rest
  else (curName, curValue) : replaceOrAdd (name, value) rest

resetCompton :: IO ()
resetCompton = getComptonPID >>= sendSIGUSR1

parseProps :: IO String
parseProps = callXDOTool >>= callXProps

killAndLaunchCompton :: String -> IO ()
killAndLaunchCompton configPath =
  getComptonPID >>= kill >> launchCompton configPath

getResetOption :: String -> [Entry] -> IO ()
getResetOption configPath entries = case find (\(name, _) -> name == CS.c_paintOnOverlay) entries of
  -- TODO Check if paint-on-overlay defaults to false...
  Nothing                  -> resetCompton
  Just (_, Enabled True) -> oldReset
  Just (_, Enabled False) -> resetCompton
  Just (_, _)              -> error "Configuration problem - paint on overlay is not of boolean type"
  where oldReset = killAndLaunchCompton configPath

-- TODO Maybe make everything use the text module?
readComptonFile :: String -> IO [Entry]
readComptonFile configPath = parseComptonFile
  . unpack
          <$> readFile configPath

comptonUpdate :: ([Entry] -> [Entry]) -> ConsReadT ()
comptonUpdate updateFunc = do
  configPath <- askConfigPath
  newComptonFile <- liftIO $ updateFunc <$> readComptonFile configPath
  liftIO
    $ writeComptonConfig configPath newComptonFile
    >> getResetOption configPath newComptonFile


uncaseActiveWin :: CurrentWindow -> IdentifyBy -> IO (String, Comparer)
uncaseActiveWin SelectActiveWindow identify = do
  windowIdentifier <- parseWindowIdentifier $ windowIdentifierGetter identify
  return (windowIdentifier, Equal)
  where parseWindowIdentifier = \getter -> parseXpropOutput . getter . lines <$> parseProps

uncaseActiveWin (NoActiveWindowSelect windowName matcher sensitivity) identify = do
  return (windowName, comparer)
  where comparer = nameMatchComparer matcher sensitivity

opacityFlow :: WinArg -> ConsReadT Perform
opacityFlow flowArg = do
  (windowName, comparer) <- liftIO
    $ uncaseActiveWin (operateOnActiveWin flowArg) identifier
  return $ ConfigUpdate (changeOpaciteeeh comptonSelector comparer (TP.opacity flowArg) windowName)
  where comptonSelector = windowIdentifierSelector $ identifier
        identifier = identifyWindowWith flowArg

flagFlow :: FlagArg -> ConsReadT Perform
flagFlow flowArg = return $
  case flagChangeAction flowArg of
    ToggleFlag -> ConfigUpdate $ flipEnabledBool flagName
    SetFlag    -> ConfigUpdate $ setEnabledBool flagName
    UnsetFlag  -> ConfigUpdate $ unsetEnabledBool flagName
    ListFlags  -> PrintList CS.booleanEntries
    where flagName = selectedFlag flowArg

wizardFlow :: WizardArg -> ConsReadT Perform
wizardFlow flowArg = do
  configPath <- askConfigPath
  config <- liftIO $ readComptonFile configPath
  newConfig <- liftIO $ runWizardSteps
    (case frontend flowArg of
      TerminalFrontend -> Term.launchSelect
      DMenuFrontend    -> Dmenu.launchSelect)
    Initial
    config
  return $ RunWizard $ newConfig

runWizardSteps :: ([(String, WizardState)] -> IO WizardState) -> WizardState -> [Entry]-> IO [Entry]
runWizardSteps frontend wizState entries = do
  wizardChoice <- frontend $ wizardStep wizState entries
  case wizardChoice of
    Exit        -> return entries
    SaveAndExit -> return entries
    other       -> runWizardSteps frontend other entries

restartModeFlow :: ConsReadT Perform
restartModeFlow = do
  configPath <- askConfigPath
  comptonConfig <- liftIO $ readComptonFile configPath
  return $ Restart $ getResetOption configPath comptonConfig

killCompton :: IO ()
killCompton = getComptonPID >>= kill

askConfigPath :: ConsReadT String
askConfigPath = configurationPath <$> ask

chooseProgramFlow :: ConsReadT ()
chooseProgramFlow = do
  progMode <- programMode <$> ask
  actionToTake <- case progMode of
    OpacityMode arguments -> opacityFlow arguments
    FlagMode arguments    -> flagFlow arguments
    KillMode              -> return Kill
    RestartMode           -> restartModeFlow
    WizardMode arguments  -> wizardFlow arguments
  takeAction actionToTake

takeAction :: Perform -> ConsReadT ()
takeAction actionToTake =
  case actionToTake of
    ConfigUpdate function -> comptonUpdate function
    PrintList strings     -> liftIO $ foldr1 (>>) $ map putStrLn strings
    Kill                  -> liftIO killCompton
    Restart ioAction      -> liftIO ioAction
    RunWizard newEntries  -> takeAction $ ConfigUpdate (\_ -> newEntries)

defaultMain :: IO ()
defaultMain = parseCommandLine
  >>= runReaderT (runConsArgRead chooseProgramFlow)
