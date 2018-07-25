module CommandLineParser
  ( parseCommandLine
  , WinArg (..)
  , CurrentWindow (..)
  , IdentifyBy (..)
  , ProgramMode (..)
  , FlagArg (..)
  , FlagChangeAction (..)
  , ConsoleArguments (..)
  ) where

import           ComptonStatic
import           Control.Applicative ((<**>), (<|>))
import           Data.List           (find)
import           Data.Semigroup      ((<>))
import           Options.Applicative (Parser, ParserInfo, auto, execParser,
                                      flag, flag', fullDesc, header, help,
                                      helper, info, long, option, progDesc,
                                      short, strOption, value)

defaultConfigPath :: String
-- TODO Change this to relative path
defaultConfigPath = "/home/bmiww/.config/compton.conf"
-- defaultConfigPath = "~/.config/compton.conf"

-- TODO Move all of these - they should belong to the consumer or service not the parser
data CurrentWindow = NoActiveWindowSelect | SelectActiveWindow
data ShouldUseDmenu = NoDmenu | UseDmenu
data IdentifyBy = ClassName | WindowName
data FlagChangeAction = ToggleFlag | SetFlag | UnsetFlag | ListFlags
-- TODO Rename flagmode to something else,
-- its heavily interfering with the library names
data ProgramMode = WindowMode WinArg | FlagMode FlagArg | DMenuMode

data ConsoleArguments = ConsoleArguments
  { programMode       :: ProgramMode
  , configurationPath :: String
  }

data WinArg = WinArg
  { operateOnActiveWin :: CurrentWindow
  , identifyWindowWith :: IdentifyBy
  , opacity            :: Integer
  }

-- TODO Flag mode also needs config path - so add it somewhere higher around the part where
-- We choose the mode
data FlagArg = FlagArg
  { selectedFlag     :: String
  , flagChangeAction :: FlagChangeAction
  }

parseCommandLine :: IO ConsoleArguments
parseCommandLine = execParser
  $ info
  (comptrollerOptions <**> helper)
  (fullDesc)

comptrollerOptions :: Parser ConsoleArguments
comptrollerOptions = ConsoleArguments
  <$> (windowModeArgs <|> flagModeArgs)
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

-- flagModeFlag :: Parser String
-- flagModeFlag = strOption
--   ( short 'F'
--   <> help "Flag mode - use to toggle or set values for true/false options"
--   )
flagModeFlag :: Parser String
flagModeFlag = checkOption <$> strOption
  ( short 'F'
  <> help "Flag mode - use to toggle or set values for true/false options"
  )
  where checkOption = \flagName -> case find (== flagName) booleanEntries of
          Nothing -> undefined
          Just _  -> flagName


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
windowModeArgs = fmap WindowMode $ WinArg
  <$> currentWindowFlag
  <*> (byClass <|> byWindowName)
  <*> opacityValue

specifyConfigPath :: Parser String
specifyConfigPath = strOption
  ( long "config-path"
  <> help "Used to specify the path to the configuration file. Default: ~/.config/compton.conf"
  <> value defaultConfigPath
  )

currentWindowFlag :: Parser CurrentWindow
currentWindowFlag = flag NoActiveWindowSelect SelectActiveWindow
  ( short 'C'
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

useDmenu :: Parser ShouldUseDmenu
useDmenu = flag NoDmenu UseDmenu
  ( short 'D'
    <> help "NOT IMPLEMENTED. Specifies to use a DMenu wizard ui for setting Compton values"
  )
