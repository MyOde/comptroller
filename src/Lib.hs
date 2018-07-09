module Lib
    ( someFunc
    ) where

import           ComptonParser          (parseComptonFile)
import           ComptonTypes           (Comparer (..), Entry,
                                         OpacityValue (..), Selector (..),
                                         Value (..), getOpacityArray)
import           Prelude                hiding (readFile)
-- import           CommandLineParser      (parseCommandLine)
import           ComptonWriter          (writeComptonConfig)
import           Constants              (defaultConfigPath)
import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)
import           Data.List              (find, lines)
import           Data.String.Utils      (startswith)
import           Data.Text              (pack, strip, unpack)
import           Data.Text.IO           (readFile)
import           Frontend               (launch)
import           GHC.IO.Handle          (hGetContents)
import           System.Environment     (getArgs)
import           System.Process         (StdStream (CreatePipe), callProcess,
                                         createProcess, proc, spawnProcess,
                                         std_out)
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

-- TODO You messed with this - it might not work properly (probably its ok)
parseProps :: IO String
parseProps = (runGetOut xdotool xdotool_Arguments
              >>= runGetOut xprop . xpropIdArguments)

runAndForget :: String -> [String] -> IO ()
runAndForget name arguments = void $ spawnProcess name arguments

findLinesBeginningWith :: [String] -> [String] -> [String]
findLinesBeginningWith lineList substrings =
  filter (\line ->
            any (\substring ->
                   startswith substring line) substrings) lineList

getPropsLines :: [String] -> [String]
getPropsLines propsString =
  findLinesBeginningWith propsString [name_identifier, class_identier]

getNameLine :: [String] -> String
getNameLine propsStrings = case find (startswith name_identifier) propsStrings of
  Just result -> result

addNewLine :: String -> String
addNewLine string = string ++ "\n"

getComptonPID :: IO String
getComptonPID
  = unpack . strip . pack
  <$> runGetOut "pidof" ["compton"]

killCompton :: String -> IO ()
killCompton ""  = return ()
killCompton pid = callProcess "kill" [pid]

launchCompton  :: String -> IO ()
launchCompton configPath = runAndForget "compton" ["-b", "--config", configPath]

randomFile = "/home/bmiww/randomfile"


replaceOrAdd :: Entry -> [Entry] -> [Entry]
replaceOrAdd (name, value) ((curName, curValue):rest) = if curName == name then (name, value):rest
                                                        else (curName, curValue) : replaceOrAdd (name, value) rest
replaceOrAdd (name, value) [] = [(name, value)]

-- TODO Move a part of this to the compton utilities or something
changeOpacity :: Integer -> String -> Value -> Value
changeOpacity opacity programName (OpacityRules rules)= OpacityRules $ newRule:rules
  where newRule = OpacityValue opacity Name Equal programName

copyConfigFile :: String -> String -> IO ()
copyConfigFile from to = callProcess "cp" [from, to]

-- TODO Try out and maybe switch to the optparse-applicative package
parseArgs :: [String] -> IO ()
-- parseArgs ["-cn", "-o", opacity] = do
parseArgs ["-c"] = do
  windowName <- getWindowName <$> parseProps
  -- comptonResult <- (extract <$> parseComptonFile defaultConfigPath)
  configString <- readFile defaultConfigPath
  let comptonResult = parseComptonFile (unpack configString)
  let newOpacityRules = changeOpacity (readInt opacity) windowName (getOpacityArray comptonResult)
  let newComptonFile = replaceOrAdd ("opacity-rule", newOpacityRules) comptonResult
  -- writeComptonConfig "/home/bmiww/randomfile" newComptonFile
  getComptonPID
    >>= killCompton
    -- >> (writeComptonConfig defaultConfigPath newComptonFile)
    >> (writeComptonConfig tempConfigPath newComptonFile)
    >> launchCompton tempConfigPath
    >> copyConfigFile tempConfigPath defaultConfigPath
    where readInt = read :: String -> Integer
          getWindowName = parseXpropOutput . getNameLine . lines
          -- TODO Dont leave undefined here
          extract (Left err)  = undefined
          extract (Right res) = res
          opacity = "99"
          tempConfigPath = defaultConfigPath ++ "_comptroller"



someFunc :: IO ()
someFunc = do
  getArgs >>= parseArgs
  -- propsString <- parseProps
  -- comptonParseResult <- parseComptonFile defaultConfigPath
  -- case  comptonParseResult of
  --   Left errorValue -> putStrLn "Failed"
  --   Right result    -> writeComptonConfig randomFile result

  ---- to be continued
  -- writeFile log_file_path
  --   ((map parseXpropOutput
  --     (getPropsLines (lines propsString))) >>= addNewLine)
