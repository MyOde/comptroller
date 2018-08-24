module Lib
    ( defaultMain
    ) where

import           CommandLineParser
import           ComptonParser     (parseComptonFile)
import qualified ComptonStatic     as CS
import           ComptonTypes      (Comparer (..), Entry, OpacityValue (..),
                                    Selector (..), Value (..), getOpacityArray)
import           ComptonUtilities  (changeOpacity, windowIdentifierSelector)
import           ComptonWriter     (writeComptonConfig)
import           Control.Monad     (liftM, void)
import           Data.List         (find)
import           Data.Text         (unpack)
import           Data.Text.IO      (readFile)
import qualified Frontend.DMenu    as Dmenu
import qualified Frontend.Terminal as Term
import           Prelude           hiding (flip, readFile)
import           Processes         (callXDOTool, callXProps, getComptonPID,
                                    kill, launchCompton, sendSIGUSR1)
import           Wizard            (WizardState (..), wizardStep)
import           XpropParser       (getClassLine, getNameLine, parseXpropOutput)

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

paintOnOverlay = "paint-on-overlay"
getResetOption :: String -> [Entry] -> IO ()
getResetOption configPath entries = case find (\(name, _) -> name == paintOnOverlay) entries of
  -- TODO Check if paint-on-overlay defaults to false...
  Nothing                  -> resetCompton
  Just (_, Enabled result) -> if result == True then oldReset else resetCompton
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

comptonUpdate :: String -> ([Entry] -> [Entry]) -> IO ()
comptonUpdate configPath updateFunc = do
  newComptonFile <- makeNewComptonFile
  writeComptonConfig configPath newComptonFile
  getResetOption configPath newComptonFile
  where comptonResult = readComptonFile configPath
        makeNewComptonFile = updateFunc <$> comptonResult

windowModeFlow :: String -> WinArg -> IO ()
windowModeFlow configPath (WinArg (NoActiveWindowSelect windowName matcher sensitivity) windowIdentifier opacity) =
  comptonUpdate configPath updateFunc
  where updateFunc = changeOpaciteeeh comptonSelector matchingComparer opacity windowName
        comptonSelector = windowIdentifierSelector windowIdentifier
        matchingComparer = case (matcher, sensitivity) of
          (PartialMatch, SensitiveMatch)   -> Like
          (PartialMatch, InsensitiveMatch) -> LikeInsens
          (EqualMatch, SensitiveMatch)     -> Equal
          (EqualMatch, InsensitiveMatch)   -> EqualInsens
-- TODO Maybe add the possibility to specify the string comparison types here too
windowModeFlow configPath (WinArg SelectActiveWindow windowIdentifier opacity) =
  updateFunc >>= comptonUpdate configPath
  where updateFunc = changeOpaciteeeh comptonSelector Equal opacity <$> windowName
        comptonSelector = windowIdentifierSelector windowIdentifier
        windowName = parseWindowIdentifier $ windowIdentifierGetter windowIdentifier

-- TODO Loads of boilerplate
flagModeFlow :: String -> FlagArg -> IO ()
flagModeFlow configPath (FlagArg flagName ToggleFlag) =
  comptonUpdate configPath $ flipEnabledBool flagName
flagModeFlow configPath (FlagArg flagName SetFlag) =
  comptonUpdate configPath $ setEnabledBool flagName
flagModeFlow configPath (FlagArg flagName UnsetFlag) =
  comptonUpdate configPath $ unsetEnabledBool flagName
flagModeFlow configPath (FlagArg flagName ListFlags) =
  foldr1 (>>) $ map putStrLn CS.booleanEntries

wizardFlow :: String -> WizardArg -> IO ()
wizardFlow configPath (WizardArg DMenuFrontend)    =
  readComptonFile configPath >>=
  runWizardSteps (Dmenu.launchSelect) Initial
wizardFlow configPath (WizardArg TerminalFrontend) =
  readComptonFile configPath >>=
  runWizardSteps (Term.launchSelect) Initial

runWizardSteps :: ([(String, WizardState)] -> IO WizardState) -> WizardState -> [Entry]-> IO ()
runWizardSteps frontend wizState entries = do
  wizardChoice <- frontend $ wizardStep wizState entries
  case wizardChoice of
    Exit        -> putStrLn "Exit"
    SaveAndExit -> putStrLn "SaveAndExit"
    other       -> runWizardSteps frontend other entries

restartModeFlow :: String -> IO ()
restartModeFlow configPath =
  comptonResult >>= getResetOption configPath
  where comptonResult = parseComptonFile . unpack
          <$> readFile configPath

killModeFlow :: IO ()
killModeFlow = getComptonPID >>= kill

chooseProgramFlow :: ConsoleArguments -> IO ()
chooseProgramFlow (ConsoleArguments (OpacityMode arguments) configPath) = windowModeFlow configPath arguments
chooseProgramFlow (ConsoleArguments (FlagMode arguments) configPath)   = flagModeFlow configPath arguments
chooseProgramFlow (ConsoleArguments (RestartMode) configPath)   = restartModeFlow configPath
chooseProgramFlow (ConsoleArguments (KillMode) configPath)   = killModeFlow
chooseProgramFlow (ConsoleArguments (WizardMode arguments) configPath)   = wizardFlow configPath arguments

defaultMain :: IO ()
defaultMain = parseCommandLine >>= chooseProgramFlow
