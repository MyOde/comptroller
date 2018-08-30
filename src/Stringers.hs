module Stringers where

ppPrepend :: Show a => Int -> a -> String -> String
ppPrepend longestVal val word = stringVal ++ spaces ++ word
  where stringVal = show val
        numberOfSpaces = longestVal - length stringVal
        spaces = map (\_->' ') [0..numberOfSpaces]
