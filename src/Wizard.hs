module Wizard where

import qualified Compton.Static as CS
import           Compton.Types   (ComptonMap, Entry, Value (..))
import           Data.Map.Strict (filterWithKey, toList)

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

-- TODO You will probably update this to use some state monad?
wizardStep :: WizardStep -> ComptonMap -> [(String, WizardStep)]
wizardStep Initial _ =
  [ changeFlag
  , changeNumeric
  , saveAndExit
  , exit
  ]

wizardStep ChooseFlagEntry entries = dynEntries ++ [saveAndExit, exit]
  -- TODO quick and dirty patch to filter by the keys in the compton map
  where entriesWithValues = filterWithKey
                            (\name _ -> any
                              (\static -> static == name)
                              CS.booleanEntries) entries
        dynEntries =
          fmap
          (\(entryName, entryValue) ->
          -- (\entryName entryValue ->
             let displayName = (dirtyBoolExtract entryValue) ++ " " ++ entryName in
               (displayName, ChangeFlag entryName)
          )
          -- TODO also quite dirty - maybe its possible to fmap or traverse/fold directly into a list type
          $ toList entriesWithValues

dirtyBoolExtract :: Value -> String
dirtyBoolExtract (Enabled False) = "T"
dirtyBoolExtract (Enabled True)  = "F"
