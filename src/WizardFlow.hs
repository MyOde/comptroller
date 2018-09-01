module WizardFlow where

import           Compton.Static           as CS
import           Compton.Types            (ComptonMap, Entry)
import           Compton.Utilities        (changeTextValue, flipEnabledBool,
                                           killAndLaunchCompton, replaceNumber)
import           Compton.Writer           (writeComptonConfig)
import           Control.Monad.State.Lazy (MonadIO, MonadState, StateT,
                                           execStateT, get, lift, liftIO, put)
import qualified Frontend.DMenu           as Dmenu
import qualified Frontend.Terminal        as Term
import           Frontend.Types           (Frontend, choice, input)
import           Numeric                  (showFFloat)
import           Processes                (deleteFile)
import           Terminal.Types
import           TypeMap
-- TODO Consider merghing the Wizard file and this one?
-- Or leave this one as is - since its mostly dealing with IO
import           Wizard                   (WizardStep (..), wizardStep)

type WizardStateT a = StateT ComptonMap ConsReadT a

makeTempPath :: String -> String
makeTempPath path = path ++ "_comptrol_temp"

putW :: ComptonMap -> WizardStateT ()
putW state = do
  configPath <- makeTempPath <$> lift askConfigPath
  liftIO $ writeComptonConfig configPath state
  liftIO $ killAndLaunchCompton configPath
  liftIO $ deleteFile configPath
  put state

launchRegularConfig :: WizardStateT ()
launchRegularConfig = do
  configPath <- lift askConfigPath
  liftIO $ killAndLaunchCompton configPath

wizardFlow :: WizardArg -> ComptonMap -> ConsReadT ()
wizardFlow flowArg initialEntries = do
  configPath <- askConfigPath
  execStateT wizardStepper initialEntries
  return ()
  where wizardStepper
          = runWizardSteps
            (case frontend flowArg of
                TerminalFrontend -> Term.userInterface
                DMenuFrontend    -> Dmenu.userInterface)
            Initial

runWizardSteps :: Frontend WizardStep -> WizardStep -> WizardStateT ()
runWizardSteps frontend wizState = do
  entries <- get
  wizardChoice <- liftIO $ choice frontend $ wizardStep wizState entries
  case wizardChoice of
    Exit                -> do
      launchRegularConfig
    SaveAndExit         -> do
      configPath <- lift askConfigPath
      liftIO $ writeComptonConfig configPath entries
      launchRegularConfig
    ChangeFlag flagName -> do
      (putW $ flipEnabledBool flagName entries)
        >> runWizardSteps frontend ChooseFlagEntry
    InputNumber entryName -> do
      -- TODO Getting too verbose here
      newValue <- liftIO $ read' <$> (input frontend $ "Editing: " ++ entryName)
      (putW $ replaceNumber entryName newValue entries)
        >> runWizardSteps frontend ChooseNumberEntry
      where read' = read :: String -> Double
    EnumValueChange entryName value -> do
      (putW $ changeTextValue entryName value entries)
        >> runWizardSteps frontend ChooseEnumEntry
    Back next           -> runWizardSteps frontend next
    other               -> runWizardSteps frontend other
