module Terminal.Types where

data EqualityMatcher
  = EqualMatch
  | PartialMatch
data SensitivityMatcher
  = SensitiveMatch
  | InsensitiveMatch
data IdentifyBy
  = ClassName
  | WindowName

data WizardFrontend
  = DMenuFrontend
  | TerminalFrontend

data WizardArg
  = WizardArg
  { frontend :: WizardFrontend
  }

data CurrentWindow
  = NoActiveWindowSelect String EqualityMatcher SensitivityMatcher
  | SelectActiveWindow
data FlagChangeAction
  = ToggleFlag
  | SetFlag
  | UnsetFlag
  | ListFlags
-- TODO Rename flagmode to something else,
-- its heavily interfering with the library names

data ProgramMode
  = OpacityMode WinArg
  | FlagMode FlagArg
  | RestartMode
  | KillMode
  | WizardMode WizardArg

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
