{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WizardFlow where

import           Compton.Static           as CS
import           Control.Monad.State.Lazy (MonadIO, MonadState, StateT,
                                           execStateT, get, lift, liftIO, put)
import qualified Frontend.DMenu           as Dmenu
import qualified Frontend.Terminal        as Term
import           Terminal.Types
import           TypeMap
-- TODO Consider merghing the Wizard file and this one?
-- Or leave this one as is - since its mostly dealing with IO
import           Compton.Types            (ComptonMap, Entry)
import           Compton.Utilities        (changeTextValue, flipEnabledBool,
                                           killAndLaunchCompton, replaceNumber)
import           Compton.Writer           (writeComptonConfig)
import           Frontend.Types           (Frontend, choice, input)
import           Numeric                  (showFFloat)
import           Wizard                   (WizardStep (..), wizardStep)

-- newtype WizardStateT a = WizardStateT
--   { runWizardState :: StateT [Entry] ConsReadT a
--   } (Applicative, Monad, Functor, MonadState [Entry], MonadIO)
type WizardStateT a = StateT ComptonMap ConsReadT a

makeTempPath :: String -> String
makeTempPath path = path ++ "_comptrol_temp"

putW :: ComptonMap -> WizardStateT ()
putW state = do
  configPath <- makeTempPath <$> lift askConfigPath
  liftIO $ writeComptonConfig configPath state
  liftIO $ killAndLaunchCompton configPath
  put state

wizardFlow :: WizardArg -> ComptonMap -> ConsReadT ComptonMap
wizardFlow flowArg initialEntries = do
  configPath <- askConfigPath
  newConfig <- execStateT wizardStepper initialEntries
  return $ newConfig
  where wizardStepper
          = runWizardSteps
            (case frontend flowArg of
                TerminalFrontend -> Term.userInterface
                DMenuFrontend    -> Dmenu.userInterface)
            Initial

-- runWizardSteps :: ([(String, WizardStep)] -> IO WizardStep) -> WizardStep -> WizardStateT ()
runWizardSteps :: Frontend WizardStep -> WizardStep -> WizardStateT ()
runWizardSteps frontend wizState = do
  entries <- get
  wizardChoice <- liftIO $ choice frontend $ wizardStep wizState entries
  case wizardChoice of
    Exit                -> return ()
    SaveAndExit         -> return ()
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
    other       -> runWizardSteps frontend other
