module Compton.Utilities where

import           Compton.Static as CS
import           Compton.Types
import           Prelude        hiding (flip)

-------------------------------------------
-- TODO DANGER ZONE
-------------------------------------------

changeOpaciteeeh :: Selector -> Comparer -> Integer -> String -> [Entry] -> [Entry]
changeOpaciteeeh selector comparer opacity windowName = replaceValue thing CS.c_opacityRule
  where thing = ehThisIsBad selector comparer opacity windowName

ehThisIsBad :: Selector -> Comparer -> Integer -> String -> Maybe Entry -> Entry
ehThisIsBad selector comparer opacity windowName (Just (_, OpacityRules imAFuckOffValue)) =
  (CS.c_opacityRule, changeOpacity opacity selector comparer windowName imAFuckOffValue)
ehThisIsBad selector comparer opacity windowName (Nothing) =
  (CS.c_opacityRule, changeOpacity opacity selector comparer windowName [])

changeOpacity :: Integer -> Selector -> Comparer -> String -> [OpacityValue] -> Value
changeOpacity opacity selector comparer programName rules = OpacityRules $ newRule:rules
  where newRule = OpacityValue opacity selector comparer programName

-------------------------------------------
-- TODO DANGER ZONE
-------------------------------------------

replaceValue :: (Maybe Entry -> Entry) -> String -> [Entry] -> [Entry]
replaceValue applyChange entryName entries = replaceValue' applyChange $ breakage
  where breakage = (break (\(name, _) -> name == entryName) entries)

replaceValue' :: (Maybe Entry -> Entry) -> ([Entry], [Entry]) -> [Entry]
replaceValue' applyChange (miss, hit:rest) = miss ++ (applyChange $ Just hit):rest
replaceValue' applyChange (miss, [])       = miss ++ [(applyChange Nothing)]

flipEnabledBool :: String -> [Entry] -> [Entry]
flipEnabledBool flagName = replaceValue (flip flagName) flagName

flip :: String -> Maybe Entry -> Entry
flip flagName (Just (name, Enabled bool)) = (name, Enabled $ not bool)
-- TODO some default value flipping???
flip flagName Nothing                     = undefined

setEnabledBool :: String -> [Entry] -> [Entry]
setEnabledBool flagName = replaceValue set flagName
  where set = \_ -> (flagName, Enabled True)

unsetEnabledBool :: String -> [Entry] -> [Entry]
unsetEnabledBool flagName = replaceValue unset flagName
  where unset = \_ -> (flagName, Enabled False)
