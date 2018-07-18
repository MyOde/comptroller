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
import           Control.Monad     (liftM, void)
import           Data.List         (find)
import           Data.Text         (unpack)
import           Data.Text.IO      (readFile)
import           Frontend          (launch)
import           Prelude           hiding (readFile)
import           Processes         (callXDOTool, callXProps, getComptonPID,
                                    kill, launchCompton, sendSIGUSR1)
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
  Nothing                  -> resetCompton
  Just (_, Enabled result) -> if result == True then oldReset else resetCompton
  Just (_, _)              -> error "Configuration weirdness - paint on overlay is not of boolean type"
  where oldReset = killAndLaunchCompton configPath

changeOpaciteeeh :: Selector -> Integer -> String -> [Entry] -> [Entry]
changeOpaciteeeh selector opacity windowName allValues = replaceOrAdd ("opacity-rule", newOpacityArray) allValues
  where opacityArray = getOpacityArray allValues
        newOpacityArray = changeOpacity opacity selector windowName opacityArray

actOnArguments :: ConsArg -> IO ()
actOnArguments (ConsArg NoActiveWindowSelect _ _ _) = error "NOT IMPLEMENTED"
actOnArguments (ConsArg SelectActiveWindow windowIdentifier opacity configPath) =
  newComptonFile
  >>= writeComptonConfig configPath
  >> newComptonFile
  >>= getResetOption configPath
  where windowName = parseWindowIdentifier $ windowIdentifierGetter windowIdentifier
        comptonResult = parseComptonFile . unpack
          <$> readFile configPath
        newComptonFile = changeOpaciteeeh comptonSelector opacity
          <$> windowName <*> comptonResult
        comptonSelector = windowIdentifierSelector windowIdentifier

defaultMain :: IO ()
defaultMain = parseCommandLine >>= actOnArguments
