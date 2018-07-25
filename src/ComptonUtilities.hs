module ComptonUtilities where

import           CommandLineParser (IdentifyBy (..))
import           ComptonTypes

changeOpacity :: Integer -> Selector -> Comparer -> String -> [OpacityValue] -> Value
changeOpacity opacity selector comparer programName rules = OpacityRules $ newRule:rules
  where newRule = OpacityValue opacity selector comparer programName

windowIdentifierSelector :: IdentifyBy -> Selector
windowIdentifierSelector ClassName  = Class
windowIdentifierSelector WindowName = Name
