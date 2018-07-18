module CommandLineParser
  ( parseCommandLine
  , ConsArg (..)
  , CurrentWindow (..)
  , IdentifyBy (..)
  ) where

import           Control.Applicative ((<**>), (<|>))
import           Data.Semigroup      ((<>))
import           Options.Applicative (Parser, ParserInfo, auto, execParser,
                                      flag, flag', fullDesc, header, help,
                                      helper, info, long, option, progDesc,
                                      short, strOption, value)

defaultConfigPath :: String
-- TODO Change this to relative path
defaultConfigPath = "/home/bmiww/.config/compton.conf"
-- defaultConfigPath = "~/.config/compton.conf"

data CurrentWindow = NoActiveWindowSelect | SelectActiveWindow
data ShouldUseDmenu = NoDmenu | UseDmenu
data IdentifyBy = ClassName | WindowName

data ConsArg = ConsArg
  { operateOnActiveWin :: CurrentWindow
  , identifyWindowWith :: IdentifyBy
  , opacity            :: Integer
  , configPath         :: String
  }

parseCommandLine :: IO ConsArg
parseCommandLine = execParser
  $ info
  (comptrollerOptions <**> helper)
  ( fullDesc
    <> progDesc "This here be the comptroller"
    <> header "OFF WITH 'IS HEAD"
  )

comptrollerOptions :: Parser ConsArg
comptrollerOptions = ConsArg
  <$> currentWindowFlag
  <*> (byClass <|> byWindowName)
  <*> opacityValue
  <*> specifyConfigPath

specifyConfigPath :: Parser String
specifyConfigPath = strOption
  ( long "config-path"
  <> help "Used to specify the path to the configuration file. Default: ~/.config/compton.conf"
  <> value defaultConfigPath
  )

currentWindowFlag :: Parser CurrentWindow
currentWindowFlag = flag NoActiveWindowSelect SelectActiveWindow
  ( short 'C'
    <> help "Will select the currently active window"
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
