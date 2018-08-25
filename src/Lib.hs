{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lib
    ( defaultMain
    ) where

import           CommandLineParser    as Clp
import           Compton.Parser       (parseComptonFile)
import qualified Compton.Static       as CS
import           Compton.Types        (Comparer (..), Entry, OpacityValue (..),
                                       Selector (..), Value (..),
                                       getOpacityArray)
import           Compton.Utilities    (changeOpacity, windowIdentifierSelector)
import           Compton.Writer       (writeComptonConfig)
import           Control.Monad     (liftM, void)
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
import           Wizard            (WizardState (..), wizardStep)
import           XpropParser          (getClassLine, getNameLine,
                                       parseXpropOutput)

paintOnOverlay = "paint-on-overlay"

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

parseWindowIdentifier :: ([String] -> String) -> IO String
parseWindowIdentifier getter = parseXpropOutput . getter . lines <$> parseProps

windowIdentifierGetter :: IdentifyBy -> ([String] -> String)
windowIdentifierGetter ClassName  = getClassLine
windowIdentifierGetter WindowName = getNameLine

getResetOption :: String -> [Entry] -> IO ()
getResetOption configPath entries = case find (\(name, _) -> name == paintOnOverlay) entries of
  -- TODO Check if paint-on-overlay defaults to false...
  Nothing                  -> resetCompton
  Just (_, Enabled True) -> oldReset
  Just (_, Enabled False) -> resetCompton
  Just (_, _)              -> error "Configuration problem - paint on overlay is not of boolean type"
  where oldReset = killAndLaunchCompton configPath

changeOpaciteeeh :: Selector -> Comparer -> Integer -> String -> [Entry] -> [Entry]
changeOpaciteeeh selector comparer opacity windowName = replaceValue thing CS.c_opacityRule
  where thing = ehThisIsBad selector comparer opacity windowName

ehThisIsBad :: Selector -> Comparer -> Integer -> String -> Maybe Entry -> Entry
ehThisIsBad selector comparer opacity windowName (Just (_, OpacityRules imAFuckOffValue)) =
  (CS.c_opacityRule, changeOpacity opacity selector comparer windowName imAFuckOffValue)
ehThisIsBad selector comparer opacity windowName (Nothing) =
  (CS.c_opacityRule, changeOpacity opacity selector comparer windowName [])

replaceValue :: (Maybe Entry -> Entry) -> String -> [Entry] -> [Entry]
replaceValue applyChange entryName entries = replaceValue' applyChange $ breakage
  where breakage = (break (\(name, _) -> name == entryName) entries)

replaceValue' :: (Maybe Entry -> Entry) -> ([Entry], [Entry]) -> [Entry]
replaceValue' applyChange (miss, hit:rest) = miss ++ (applyChange $ Just hit):rest
replaceValue' applyChange (miss, [])       = miss ++ [(applyChange Nothing)]

flipEnabledBool :: String -> [Entry] -> [Entry]
flipEnabledBool flagName = replaceValue (flip flagName) flagName

flip :: String -> Maybe Entry -> Entry
flip flagName (Just (name, Enabled bool)) = (name, Enabled $ not bool)
-- TODO some default value flipping???
flip flagName Nothing                     = undefined

setEnabledBool :: String -> [Entry] -> [Entry]
setEnabledBool flagName = replaceValue set flagName
  where set = \_ -> (flagName, Enabled True)

unsetEnabledBool :: String -> [Entry] -> [Entry]
unsetEnabledBool flagName = replaceValue unset flagName
  where unset = \_ -> (flagName, Enabled False)

readComptonFile :: String -> IO [Entry]
readComptonFile configPath = parseComptonFile . unpack
          <$> readFile configPath

comptonUpdate :: ([Entry] -> [Entry]) -> ConsReadT ()
comptonUpdate updateFunc = do
  configPath <- askConfigPath
  newComptonFile <- liftIO $ updateFunc <$> readComptonFile configPath
  liftIO
    $ writeComptonConfig configPath newComptonFile
    >> getResetOption configPath newComptonFile

nameMatchComparer :: EqualityMatcher -> SensitivityMatcher -> Comparer
nameMatchComparer PartialMatch SensitiveMatch   = Like
nameMatchComparer PartialMatch InsensitiveMatch = LikeInsens
nameMatchComparer EqualMatch SensitiveMatch     = Equal
nameMatchComparer EqualMatch InsensitiveMatch   = EqualInsens

uncaseActiveWin :: CurrentWindow -> IdentifyBy -> IO (String, Comparer)
uncaseActiveWin SelectActiveWindow identify = do
  windowIdentifier <- parseWindowIdentifier $ windowIdentifierGetter $ identify
  return (windowIdentifier, Equal)

uncaseActiveWin (NoActiveWindowSelect windowName matcher sensitivity) identify = do
  return (windowName, comparer)
  where comparer = nameMatchComparer matcher sensitivity

opacityFlow :: WinArg -> ConsReadT Perform
opacityFlow flowArg = do
  (windowName, comparer) <- liftIO
    $ uncaseActiveWin (operateOnActiveWin flowArg) identifier
  return $ ConfigUpdate (changeOpaciteeeh comptonSelector comparer (Clp.opacity flowArg) windowName)
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

newtype ConsReadT a = ConsReadT
  { runConsArgRead :: ReaderT ConsoleArguments IO a
  } deriving (Applicative, Monad, Functor, MonadReader ConsoleArguments, MonadIO)

askConfigPath :: ConsReadT String
askConfigPath = configurationPath <$> ask

data Perform
  = ConfigUpdate ([Entry] -> [Entry])
  | PrintList [String]
  | Kill
  -- TODO You're asking to be hurt
  | Restart (IO ())
  | RunWizard [Entry]

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
