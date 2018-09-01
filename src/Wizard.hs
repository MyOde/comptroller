module Wizard where

import qualified Compton.Static as CS
import           Compton.Types   (ComptonMap, Entry, Value (..))
import           Data.Map.Strict (filterWithKey, toList, (!?))
import           Numeric         (showFFloat)
import           Stringers       (ppPrepend)

data WizardStep
  = Initial
  | ChooseFlagEntry
  | ChooseNumberEntry
  | ChooseEnumEntry
  | ChangeFlag String
  | InputNumber String
  | Back WizardStep
  | SaveAndExit
  | Exit

saveAndExit :: (String, WizardStep)
saveAndExit = ("Save & exit", SaveAndExit)

exit :: (String, WizardStep)
exit = ("Exit", Exit)

back :: WizardStep -> (String, WizardStep)
back next = ("Back", Back next)

changeNumeric :: (String, WizardStep)
changeNumeric = ("Change numeric", ChooseNumberEntry)

changeFlag :: (String, WizardStep)
changeFlag = ("Toggle flag", ChooseFlagEntry)

persistentChoices :: WizardStep -> [(String, WizardStep)]
persistentChoices next = [back next, saveAndExit, exit]

-- TODO You will probably update this to use some state monad?
wizardStep :: WizardStep -> ComptonMap -> [(String, WizardStep)]
wizardStep Initial _ =
  [ changeFlag
  , changeNumeric
  , saveAndExit
  , exit
  ]

-- TODO This will only select the existing values
-- If a user does not have some values specified in their config
-- They will not appear in the wizard
wizardStep ChooseFlagEntry entries = dynEntries ++ persistentChoices Initial
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

-- TODO Make a spacer that checks the length of text in order to space values depending on longest
-- Will most definitely fail on non-monospace fonts
wizardStep ChooseNumberEntry entries = pretty ++ persistentChoices Initial
  where numbersWithValues = fmap (mixWithNumberValue entries) CS.floatingEntries
        maxLength = maximum . map (stringLength . third) $ numbersWithValues
        pretty = map (\(name, step, val) -> (ppPrepend maxLength val name, step)) numbersWithValues

-- wizardStep ChooseEnumEntry entries = pretty ++ persistentChoices Initial

third :: (a,b,c) -> c
third (_,_,val) = val

stringLength :: Show a => a -> Int
stringLength = length . show

mixWithNumberValue :: ComptonMap -> String -> (String, WizardStep, Value)
mixWithNumberValue entries name = case entries !? name of
  Just result -> (name, InputNumber name, result)
  -- TODO Fake magic 0. Should find default values for all of these
  Nothing     -> (name, InputNumber name, Floating 0)


dirtyBoolExtract :: Value -> String
dirtyBoolExtract (Enabled False) = "F"
dirtyBoolExtract (Enabled True)  = "T"
