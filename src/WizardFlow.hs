{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WizardFlow where

import           Control.Monad.State.Lazy (MonadIO, MonadState, StateT,
                                           execStateT, get, liftIO, put)
import qualified Frontend.DMenu           as Dmenu
import qualified Frontend.Terminal        as Term
import           Terminal.Types
import           TypeMap
-- TODO Consider merghing the Wizard file and this one?
-- Or leave this one as is - since its mostly dealing with IO
import           Compton.Types            (ComptonMap, Entry)
import           Wizard                   (WizardStep (..), wizardStep)


-- newtype WizardStateT a = WizardStateT
--   { runWizardState :: StateT [Entry] ConsReadT a
--   } (Applicative, Monad, Functor, MonadState [Entry], MonadIO)
type WizardStateT a = StateT ComptonMap ConsReadT a

wizardFlow :: WizardArg -> ComptonMap -> ConsReadT ComptonMap
wizardFlow flowArg initialEntries = do
  configPath <- askConfigPath
  newConfig <- execStateT wizardStepper initialEntries
  return $ newConfig
  where wizardStepper
          = runWizardSteps
            (case frontend flowArg of
                TerminalFrontend -> Term.launchSelect
                DMenuFrontend    -> Dmenu.launchSelect)
            Initial

runWizardSteps :: ([(String, WizardStep)] -> IO WizardStep) -> WizardStep -> WizardStateT ()
runWizardSteps frontend wizState = do
  entries <- get
  wizardChoice <- liftIO $ frontend $ wizardStep wizState entries
  case wizardChoice of
    Exit        -> put entries
    SaveAndExit -> put entries
    other       -> runWizardSteps frontend other
