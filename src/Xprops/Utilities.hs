module Xprops.Utilities
  ( getClassLine
  , getNameLine
  , windowIdentifierGetter
  ) where

import           Data.List         (find)
import           Data.String.Utils (startswith)
import           Terminal.Types

class_identier = "WM_CLASS"
name_identifier = "WM_NAME"

getNameLine :: [String] -> String
getNameLine propsStrings = case find (startswith name_identifier) propsStrings of
  Just result -> result
  Nothing     -> error "Window missing the name property"

getClassLine :: [String] -> String
getClassLine propsString = case find (startswith class_identier) propsString of
  Just result -> result
  Nothing     -> error "Window missing the class property"

windowIdentifierGetter :: IdentifyBy -> ([String] -> String)
windowIdentifierGetter ClassName  = getClassLine
windowIdentifierGetter WindowName = getNameLine
