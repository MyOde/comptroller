module Lib
    ( defaultMain
    ) where

import           Compton.Parser       (readComptonFile)
import qualified Compton.Static       as CS
import           Compton.Types        (Comparer (..), ComptonMap)
import           Compton.Utilities    (changeOpaciteeeh, flipEnabledBool,
                                       getComptonPID, getResetOption,
                                       setEnabledBool, unsetEnabledBool)
import           Compton.Writer       (writeComptonConfig)
import           Control.Monad.Reader (ask, liftIO, runReaderT)
import           Processes            (callXDOTool, callXProps, kill)
import           Terminal.Parser      as TP
import           TypeMap              (ConsReadT, askConfigPath,
                                       nameMatchComparer, runConsArgRead,
                                       windowIdentifierSelector)
import           WizardFlow           (wizardFlow)
import           Xprops.Parser        (parseXpropOutput)
import           Xprops.Utilities     (windowIdentifierGetter)

data Perform
  = ConfigUpdate (ComptonMap -> ComptonMap)
  | PrintList [String]
  | Kill
  -- TODO You're asking to be hurt
  | Restart (IO ())

parseProps :: IO String
parseProps = callXDOTool >>= callXProps

comptonUpdate :: (ComptonMap -> ComptonMap) -> ConsReadT ()
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

restartModeFlow :: ConsReadT Perform
restartModeFlow = do
  configPath <- askConfigPath
  comptonConfig <- liftIO $ readComptonFile configPath
  return $ Restart $ getResetOption configPath comptonConfig

killCompton :: IO ()
killCompton = getComptonPID >>= kill

chooseProgramFlow :: ConsReadT ()
chooseProgramFlow = do
  progMode <- programMode <$> ask
  actionToTake <- case progMode of
    OpacityMode arguments -> opacityFlow arguments
    FlagMode arguments    -> flagFlow arguments
    KillMode              -> return Kill
    RestartMode           -> restartModeFlow
    WizardMode arguments  -> do
      configPath <- askConfigPath
      config <- liftIO $ readComptonFile configPath
      wizardFlow arguments config
      restartModeFlow
  takeAction actionToTake

takeAction :: Perform -> ConsReadT ()
takeAction actionToTake =
  case actionToTake of
    ConfigUpdate function -> comptonUpdate function
    PrintList strings     -> liftIO $ foldr1 (>>) $ map putStrLn strings
    Kill                  -> liftIO killCompton
    Restart ioAction      -> liftIO ioAction

defaultMain :: IO ()
defaultMain = parseCommandLine
  >>= runReaderT (runConsArgRead chooseProgramFlow)
