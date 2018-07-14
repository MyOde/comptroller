module ComptonUtilities where

import           ComptonTypes

changeOpacity :: Integer -> Selector -> String -> Value -> Value
changeOpacity opacity selector programName (OpacityRules rules) = OpacityRules $ newRule:rules
  where newRule = OpacityValue opacity selector Equal programName
