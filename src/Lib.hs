module Lib
    ( someFunc
    ) where

import           CommandLineParser (ConsArg (..), CurrentWindow (..),
                                    IdentifyBy (..), parseCommandLine)
import           ComptonParser          (parseComptonFile)
import           ComptonTypes      (Comparer (..), Entry, OpacityValue (..),
                                    Selector (..), Value (..), getOpacityArray)
import           ComptonUtilities  (changeOpacity)
import           ComptonWriter          (writeComptonConfig)
import           Control.Monad          (void)
import           Data.List              (find, lines)
import           Data.String.Utils      (startswith)
import           Data.Text              (pack, strip, unpack)
import           Data.Text.IO           (readFile)
import           Frontend               (launch)
import           GHC.IO.Handle          (hGetContents)
import           Prelude           hiding (readFile)
import           System.Process         (StdStream (CreatePipe), callProcess,
                                    createProcess, proc, spawnProcess, std_out)
import           XpropParser            (parseXpropOutput)

class_identier = "WM_CLASS"
name_identifier = "WM_NAME"
xprop = "xprop"
xdotool = "xdotool"
xdotool_Arguments = [ "getwindowfocus" ]
log_file_path = "/home/bmiww/comproller_log"

xpropIdArguments :: String -> [String]
xpropIdArguments id = [ "-id", id ]

-- TODO Could be replaced with a function already defined in the Process library
runGetOut :: String -> [String] -> IO String
runGetOut name arguments = do
  (_, Just outHandle, _, _) <- createProcess
    (proc name arguments){ std_out = CreatePipe }
  hGetContents outHandle

parseProps :: IO String
parseProps = (runGetOut xdotool xdotool_Arguments
              >>= runGetOut xprop . xpropIdArguments)

runAndForget :: String -> [String] -> IO ()
runAndForget name arguments = void $ spawnProcess name arguments

getNameLine :: [String] -> String
getNameLine propsStrings = case find (startswith name_identifier) propsStrings of
  Just result -> result
  Nothing     -> error "Window missing the name property"

getClassLine :: [String] -> String
getClassLine propsString = case find (startswith class_identier) propsString of
  Just result -> result
  Nothing     -> error "Window missing the class property"

getComptonPID :: IO String
getComptonPID = unpack . strip . pack
  <$> runGetOut "pidof" ["compton"]

kill :: String -> IO ()
kill pid = callProcess "kill" [pid]

-- TODO Check if You need to manually do something with the handle
-- Maybe just voiding it could cause some weirdness
launchCompton  :: String -> IO ()
launchCompton configPath = runAndForget "compton" ["-b", "--config", configPath]

replaceOrAdd :: Entry -> [Entry] -> [Entry]
replaceOrAdd (name, value) ((curName, curValue):rest) = if curName == name then (name, value):rest
                                                        else (curName, curValue) : replaceOrAdd (name, value) rest
replaceOrAdd (name, value) [] = [(name, value)]

copyConfigFile :: String -> String -> IO ()
copyConfigFile from to = callProcess "cp" [from, to]

sendSIGUSR1 :: String -> IO ()
sendSIGUSR1 pid = callProcess "kill" ["-s", "SIGUSR1", pid]

resetCompton :: IO ()
resetCompton = getComptonPID >>= sendSIGUSR1

oldComptonReset :: String -> [Entry] -> IO ()
oldComptonReset configPath newComptonFile =
  getComptonPID >>= kill
  >> (writeComptonConfig tempConfigPath newComptonFile)
  >> launchCompton tempConfigPath
  >> copyConfigFile tempConfigPath configPath
  where tempConfigPath = configPath ++ "_comptroller"

parseWindowIdentifier :: ([String] -> String) -> IO String
parseWindowIdentifier getter = parseXpropOutput . getter . lines <$> parseProps

someFunc :: IO ()
someFunc = parseCommandLine >>= actOnArguments

windowIdentifierGetter :: IdentifyBy -> ([String] -> String)
windowIdentifierGetter ClassName  = getClassLine
windowIdentifierGetter WindowName = getNameLine

windowIdentifierSelector :: IdentifyBy -> Selector
windowIdentifierSelector ClassName  = Class
windowIdentifierSelector WindowName = Name

actOnArguments :: ConsArg -> IO ()
actOnArguments (ConsArg NoActiveWindowSelect _ _ _) = error "NOT IMPLEMENTED"
actOnArguments (ConsArg SelectActiveWindow windowIdentifier opacity configPath) =
  newComptonFile >>= writeComptonConfig tempConfigPath
  >> copyConfigFile tempConfigPath configPath
  >> resetCompton
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
