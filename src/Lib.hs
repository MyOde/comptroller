module Lib
    ( defaultMain
    ) where

import           CommandLineParser (ConsArg (..), CurrentWindow (..),
                                    IdentifyBy (..), parseCommandLine)
import           ComptonParser     (parseComptonFile)
import           ComptonTypes      (Comparer (..), Entry, OpacityValue (..),
                                    Selector (..), Value (..), getOpacityArray)
import           ComptonUtilities  (changeOpacity, windowIdentifierSelector)
import           ComptonWriter     (writeComptonConfig)
import           Control.Monad     (void)
import           Data.Text         (unpack)
import           Data.Text.IO      (readFile)
import           Frontend          (launch)
import           Prelude           hiding (readFile)
import           Processes         (callXDOTool, callXProps, copyFile,
                                    getComptonPID, kill, launchCompton,
                                    sendSIGUSR1)
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

actOnArguments :: ConsArg -> IO ()
actOnArguments (ConsArg NoActiveWindowSelect _ _ _) = error "NOT IMPLEMENTED"
actOnArguments (ConsArg SelectActiveWindow windowIdentifier opacity configPath) =
  newComptonFile >>= writeComptonConfig tempConfigPath
  >> copyFile tempConfigPath configPath
  >> resetCompton
  -- >> oldReset
  -- TODO Only real IO here comes from window name and compton result lines
  -- The rest should be extracted as regular functions. Maybe.
  where windowName = parseWindowIdentifier $ windowIdentifierGetter windowIdentifier
        comptonResult = parseComptonFile . unpack
          <$> readFile configPath
        comptonOpacityRules = getOpacityArray <$> comptonResult
        newOpacityRules = changeOpacity opacity comptonSelector
          <$> windowName
          <*> comptonOpacityRules
        newComptonFile = replaceOrAdd
          <$> ((,) <$> pure "opacity-rule" <*> newOpacityRules)
          <*> comptonResult
        tempConfigPath = configPath ++ "_comptroller"
        comptonSelector = windowIdentifierSelector windowIdentifier
        oldReset = killAndLaunchCompton tempConfigPath

defaultMain :: IO ()
defaultMain = parseCommandLine >>= actOnArguments
