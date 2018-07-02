{-# LANGUAGE RecordWildCards #-}
module ComptonWriter where

import           ComptonTypes
import           System.IO    (writeFile)

spaceEqual = " = "
spaceColon = " :\n{\n"
spaceColonTabbed = " :\n\t{\n"

spaceArray :: String
spaceArray = spaceEqual ++ "[\n"

addComa :: String -> String
addComa original = original ++ ","
addSemiColon :: String -> String
addSemiColon original = original ++ ";"
addNewLine :: String -> String
addNewLine original = original ++ "\n"

doubleTab :: String -> String
doubleTab original = "\t\t" ++ original

writeComptonConfig :: String -> [Entry] -> IO ()
writeComptonConfig configPath entries = writeFile configPath
  $ createComptonLines entries >>= addNewLine

createComptonLines :: [Entry] -> [String]
createComptonLines entries = map comptonLine entries

comptonLine :: Entry -> String
comptonLine (name, value) =
  name ++ typeOpening ++ stringValue ++ typeClosing ++ ";"
  where stringValue = comptonValue value
        typeOpening = valueOpeningString value
        typeClosing = valueClosingString value

valueOpeningString :: Value -> String
valueOpeningString (WinTypes _)     = spaceColon
valueOpeningString (OpacityRules _) = spaceArray
valueOpeningString (RegularRules _) = spaceArray
valueOpeningString _                = spaceEqual

valueClosingString :: Value -> String
valueClosingString (WinTypes _)     = "}"
valueClosingString (OpacityRules _) = "]"
valueClosingString (RegularRules _) = "]"
valueClosingString _                = ""

comptonValue :: Value -> String
comptonValue (Enabled val)        = show val
comptonValue (Textual val)        = "\"" ++ val ++ "\""
comptonValue (Numeric val)        = show val
comptonValue (Floating val)       = show val
comptonValue (WinTypes types)     = map winTypeLine types >>= addNewLine
comptonValue (OpacityRules rules) = map opacityLine rules
                                    >>= (addNewLine . addComa)
comptonValue (RegularRules rules) = map regularArrayLine rules
                                    >>= (addNewLine . addComa)

winTypeLine :: WinType -> String
winTypeLine WinType{..} =
  "\t" ++ name ++ spaceColonTabbed ++ argumentLines ++ "\t}\n"
  where argumentLines = (map makeWinArgument arguments)
                        -- TODO this composition order feels a bit weird
                        >>= (doubleTab . addNewLine . addSemiColon)

makeWinArgument :: WinTypeArg -> String
makeWinArgument (Opacity value) = "opacity = " ++ valueWrite value
makeWinArgument (Fade value)    = "fade = " ++ valueWrite value
makeWinArgument (Shadow value)  = "shadow = " ++ valueWrite value
makeWinArgument (Focus value)   = "focus = " ++ valueWrite value

valueWrite :: Value -> String
valueWrite (Enabled boolean) = show boolean
valueWrite (Floating double) = show double

arrayLine :: String -> String -> String -> String -> String
arrayLine prefix selectorWord compareWord valueWord =
  "\t\"" ++ prefix ++ selectorWord ++ " "
  ++ compareWord ++ " " ++ valueWord ++ "\""

regularArrayLine :: RegularValue -> String
regularArrayLine RegularValue{..} =
  arrayLine "" selectorWord comparerWord valueWord
  where selectorWord = selectorString r_selector
        comparerWord = comparerString r_comparison
        valueWord = "'" ++ r_value ++ "'"

opacityLine :: OpacityValue -> String
opacityLine OpacityValue{..} =
  arrayLine opacityWord selectorWord comparerWord valueWord
  where opacityWord = (show opacity)++ ":"
        selectorWord = selectorString selector
        comparerWord = comparerString comparison
        valueWord = "'" ++ value ++ "'"
