module Wizard where

import qualified ComptonStatic as CS
import           ComptonTypes  (Entry, Value (..))
import           Data.List     (filter)

data WizardState
  = Initial
  | ChooseFlagEntry
  | ChooseNumberEntry
  | ChangeFlag String
  | SaveAndExit
  | Exit

saveAndExit :: (String, WizardState)
saveAndExit = ("Save & exit", SaveAndExit)

exit :: (String, WizardState)
exit = ("Exit", Exit)

changeNumeric :: (String, WizardState)
changeNumeric = ("Change numeric", ChooseNumberEntry)

changeFlag :: (String, WizardState)
changeFlag = ("Toggle flag", ChooseFlagEntry)

wizardStep :: WizardState -> [Entry] -> [(String, WizardState)]
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
