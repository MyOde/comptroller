module Processes
  ( callXDOTool
  , callXProps
  , getPIDByName
  , kill
  , sendSIGUSR1
  , launchCompton
  ) where

import           Data.Text      (pack, strip, unpack)
import           System.Process (callProcess, readProcess)

xprop = "xprop"
xdotool = "xdotool"
killCommand = "kill"
xdotool_Arguments = ["getwindowfocus"]

xpropIdArguments :: String -> [String]
xpropIdArguments id = ["-id", id]

readProcess' :: FilePath -> [String] -> IO String
readProcess' a b = readProcess a b []


callXDOTool :: IO String
callXDOTool = readProcess' xdotool xdotool_Arguments

callXProps :: String -> IO String
callXProps id = readProcess' xprop $ xpropIdArguments id

getPIDByName :: String -> IO String
getPIDByName name = unpack . strip . pack
  <$> readProcess' "pidof" [name]

kill :: String -> IO ()
kill pid = callProcess killCommand [pid]

sendSIGUSR1 :: String -> IO ()
sendSIGUSR1 pid = callProcess killCommand ["-s", "SIGUSR1", pid]

launchCompton  :: String -> IO ()
launchCompton configPath = callProcess "compton" ["-b", "--config", configPath]
