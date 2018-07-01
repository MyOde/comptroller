module Lib
    ( someFunc
    ) where

import           ComptonParser     (parseComptonFile)
import           Control.Monad     ((>>=))
import           Data.List         (find, lines)
import           Data.String.Utils (startswith)
import           Frontend          (launch)
import           GHC.IO.Handle     (hGetContents)
import           System.Process    (StdStream (CreatePipe), createProcess, proc,
                                    std_out)
import           Text.Parsec.Char  (char)
import           XpropParser       (parseXpropOutput)

class_identier = "WM_CLASS"
name_identifier = "WM_NAME"
xprop = "xprop"
xdotool = "xdotool"
xdotool_Arguments = [ "getwindowfocus" ]
compton_config_path = "/home/bmiww/.config/compton.conf"
log_file_path = "/home/bmiww/comproller_log"

xpropIdArguments :: String -> [String]
xpropIdArguments id = [ "-id", id ]

runGetOut :: String -> [String] -> IO String
runGetOut name arguments = do
  (_, Just outHandle, _, _) <- createProcess
    (proc name arguments){ std_out = CreatePipe }
  hGetContents outHandle

findLinesBeginningWith :: [String] -> [String] -> [String]
findLinesBeginningWith lineList substrings =
  filter (\line ->
            any (\substring ->
                   startswith substring line) substrings) lineList

getPropsLines :: [String] -> [String]
getPropsLines propsString =
  findLinesBeginningWith propsString [name_identifier, class_identier]

addNewLine :: String -> String
addNewLine string = string ++ "\n"

someFunc :: IO ()
someFunc = do
  propsString <- (runGetOut xdotool xdotool_Arguments
                  >>= \id -> runGetOut xprop (xpropIdArguments id))

  couldBeComptonConfig <- parseComptonFile compton_config_path
  ---- to be continued
  writeFile log_file_path
    ((map parseXpropOutput
      (getPropsLines (lines propsString))) >>= addNewLine)