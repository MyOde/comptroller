module CommandLineParser
  ( parseCommandLine
  , WinArg (..)
  , CurrentWindow (..)
  , IdentifyBy (..)
  , ProgramMode (..)
  , FlagArg (..)
  , FlagChangeAction (..)
  , ConsoleArguments (..)
  , EqualityMatcher (..)
  , SensitivityMatcher (..)
  , WizardArg (..)
  , WizardFrontend (..)
  ) where

import           ComptonStatic
import           Control.Applicative ((<**>), (<|>))
import           Data.List           (find)
import           Data.Semigroup      ((<>))
import           Options.Applicative (Parser, ParserInfo, auto, execParser,
                                      flag, flag', fullDesc, header, help,
                                      helper, info, long, option, progDesc,
                                      short, strOption, value)

-- TODO Change this to relative path somewhere
defaultConfigPath :: String
defaultConfigPath = "/home/bmiww/.config/compton.conf"
-- defaultConfigPath = "~/.config/compton.conf"

-- TODO Move all of these - they should belong to the consumer or service not the parser

data CurrentWindow
  = NoActiveWindowSelect String EqualityMatcher SensitivityMatcher
  | SelectActiveWindow
data IdentifyBy
  = ClassName
  | WindowName
data FlagChangeAction
  = ToggleFlag
  | SetFlag
  | UnsetFlag
  | ListFlags
data WizardFrontend
  = DMenuFrontend
  | TerminalFrontend
-- TODO Rename flagmode to something else,
-- its heavily interfering with the library names

data ProgramMode
  = OpacityMode WinArg
  | FlagMode FlagArg
  | RestartMode
  | KillMode
  | WizardMode WizardArg
data EqualityMatcher
  = EqualMatch
  | PartialMatch
data SensitivityMatcher
  = SensitiveMatch
  | InsensitiveMatch

data ConsoleArguments
  = ConsoleArguments
  { programMode       :: ProgramMode
  , configurationPath :: String
  }
data WinArg
  = WinArg
  { operateOnActiveWin :: CurrentWindow
  , identifyWindowWith :: IdentifyBy
  , opacity            :: Integer
  }
data FlagArg
  = FlagArg
  { selectedFlag     :: String
  , flagChangeAction :: FlagChangeAction
  }

data WizardArg
  = WizardArg
  { frontend :: WizardFrontend
  }

availableModes :: Parser ProgramMode
availableModes
  = windowModeArgs
  <|> flagModeArgs
  <|> restartCompton
  <|> killCompton
  <|> wizardModeArgs

parseCommandLine :: IO ConsoleArguments
parseCommandLine = execParser $ info
  (comptrollerOptions <**> helper)
  (fullDesc)

comptrollerOptions :: Parser ConsoleArguments
comptrollerOptions = ConsoleArguments
  <$> availableModes
  <*> specifyConfigPath

flagModeArgs :: Parser ProgramMode
flagModeArgs = fmap FlagMode $ FlagArg
  <$> flagModeFlag
  <*> (toggleFlag <|> setFlag <|> unsetFlag <|> listFlags)

listFlags :: Parser FlagChangeAction
listFlags = flag' ListFlags
  ( short 'l'
  <> help "List available flag names"
  )
toggleFlag :: Parser FlagChangeAction
toggleFlag = flag' ToggleFlag
  ( short 't'
  <> help "Toggle specified flag"
  )
setFlag :: Parser FlagChangeAction
setFlag = flag' SetFlag
  ( short 's'
  <> help "Sets the specfied flag value to true"
  )
unsetFlag :: Parser FlagChangeAction
unsetFlag = flag' UnsetFlag
  ( short 'u'
  <> help "Sets the specfied flag value to false"
  )

windowModeArgs :: Parser ProgramMode
windowModeArgs = fmap OpacityMode $ WinArg
  <$> (useOpacityMode *> (currentWindowFlag <|> specifyWindow))
  <*> (byClass <|> byWindowName)
  <*> opacityValue

-- TODO Rename CurrentWindow type to SelectedWindow
specifyWindow :: Parser CurrentWindow
specifyWindow = NoActiveWindowSelect <$> specifyWindowName <*> specifyEquality <*> specifyMatchSensitivity

specifyWindowName :: Parser String
specifyWindowName = strOption
  ( short 's'
    <> help "Specify the name or class of the window to match"
  )

specifyEquality :: Parser EqualityMatcher
specifyEquality = flag EqualMatch PartialMatch
  ( short 'p'
  <> help "Creates a rule with partial string match"
  )

specifyMatchSensitivity :: Parser SensitivityMatcher
specifyMatchSensitivity = flag SensitiveMatch InsensitiveMatch
  ( short 'i'
  <> help "Compare window name with case insensitive matches"
  )

currentWindowFlag :: Parser CurrentWindow
currentWindowFlag = flag' SelectActiveWindow
  ( short 'a'
    <> help "Window mode. Change attributes for a specific or class based window"
  )

opacityValue :: Parser Integer
opacityValue = option auto
  ( short 'o'
    <> help "Specifies the opacity percentage to be applied to either the current window (-C), or ..."
  )

byClass :: Parser IdentifyBy
byClass = flag' ClassName
  ( short 'c'
    <> help "Specifies that the window class property should be used for identifying thep opacity rule"
  )

byWindowName :: Parser IdentifyBy
byWindowName = flag' WindowName
  ( short 'n'
    <> help "Specifies that the window name property should be used for identifying the opacity rule"
  )

specifyConfigPath :: Parser String
specifyConfigPath = strOption
  ( long "config-path"
  <> help "Used to specify the path to the configuration file. Default: ~/.config/compton.conf"
  <> value defaultConfigPath
  )

-- WIZARD MODE FLAGS
wizardModeArgs :: Parser ProgramMode
wizardModeArgs = fmap WizardMode $ WizardArg
  <$> (useWizardMode *> (dmenuFrontend <|> terminalFrontend))

dmenuFrontend :: Parser WizardFrontend
dmenuFrontend = flag' DMenuFrontend
  ( short 'd'
  <> help "Use a dmenu driven frontend"
  )

terminalFrontend :: Parser WizardFrontend
terminalFrontend = flag' TerminalFrontend
  ( short 't'
  <> help "Use a terminal driven frontend"
  )

-- MODE FLAGS
killCompton :: Parser ProgramMode
killCompton = flag' KillMode
  ( short 'K'
  <> help "Kills the running compton instance"
  )

restartCompton :: Parser ProgramMode
restartCompton = flag' RestartMode
  ( short 'R'
  <> help "Restart the compton process"
  )

-- TODO Figure out how to make it so that flag mode requires a string parameter
-- at the end of the argument line
-- And make it print help when Nothing provided
flagModeFlag :: Parser String
flagModeFlag = checkOption <$> strOption
  ( short 'F'
  <> help "Flag mode - use to toggle or set values for true/false options"
  )
  where checkOption = \flagName -> case find (== flagName) booleanEntries of
          Nothing -> undefined
          Just _  -> flagName

useOpacityMode :: Parser Bool
useOpacityMode = flag' True
  ( short 'O'
  <> help "Use this flag to run the program in opacity mode"
  )

useWizardMode :: Parser Bool
useWizardMode  = flag' True
  ( short 'W'
    <> help "Specifies a wizard mode for setting up compton options with realtime changes and an option to commit all the values changed"
  )
