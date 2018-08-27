module Wizard where

import qualified Compton.Static as CS
import           Compton.Types  (Entry, Value (..))
import           Data.List     (filter)

data WizardStep
  = Initial
  | ChooseFlagEntry
  | ChooseNumberEntry
  | ChangeFlag String
  | SaveAndExit
  | Exit

saveAndExit :: (String, WizardStep)
saveAndExit = ("Save & exit", SaveAndExit)

exit :: (String, WizardStep)
exit = ("Exit", Exit)

changeNumeric :: (String, WizardStep)
changeNumeric = ("Change numeric", ChooseNumberEntry)

changeFlag :: (String, WizardStep)
changeFlag = ("Toggle flag", ChooseFlagEntry)

wizardStep :: WizardStep -> [Entry] -> [(String, WizardStep)]
wizardStep Initial _ =
  [ changeFlag
  , changeNumeric
  , saveAndExit
  , exit
  ]

wizardStep ChooseFlagEntry entries = dynEntries ++ [saveAndExit, exit]
  where entriesWithValues = filter
                            (\(name, _) -> any
                              (\static -> static == name)
                              CS.booleanEntries) entries
        dynEntries =
          fmap
          (\(entryName, entryValue) ->
             let displayName = (dirtyBoolExtract entryValue) ++ " " ++ entryName in
               (displayName, ChangeFlag entryName)
          )
          entriesWithValues

dirtyBoolExtract :: Value -> String
dirtyBoolExtract (Enabled False) = "T"
dirtyBoolExtract (Enabled True)  = "F"
