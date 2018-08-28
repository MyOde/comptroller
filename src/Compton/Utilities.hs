module Compton.Utilities where

import           Compton.Static as CS
import           Compton.Types
import           Data.Map.Strict (fromList, insert, insertWith, toList, (!?))
import           Prelude        hiding (flip)

getOpacityArray :: ComptonMap -> Value
getOpacityArray entries = case entries !? CS.c_opacityRule of
  Nothing     -> OpacityRules []
  Just result -> result

changeOpaciteeeh :: Selector -> Comparer -> Integer -> String -> ComptonMap -> ComptonMap
changeOpaciteeeh selector comparer opacity windowName = changeOpacity' $ OpacityValue opacity selector comparer windowName

-- TODO This will be a lot slower than it should be
-- Also it's quite expressive due to the extra transformations just to access the convenient insert function
changeOpacity' :: OpacityValue -> ComptonMap -> ComptonMap
changeOpacity' newVal currentConf =
  insert CS.c_opacityRule newRules currentConf
  where newRules = OpacityRules $ map toOpacityValue
          $ toList
          $ insert (value newVal) newVal
          $ fromList
          $ map fromOpacityValue
          $ opacityValues
          $ getOpacityArray currentConf

fromOpacityValue :: OpacityValue -> (String, OpacityValue)
fromOpacityValue opa = (value opa, opa)

toOpacityValue :: (String, OpacityValue) -> OpacityValue
toOpacityValue (_, opa) = opa

-- TODO for now always providing true as the value to insert.
-- This might not be the default value for some boolean entries.
flipEnabledBool :: String -> ComptonMap -> ComptonMap
flipEnabledBool flagName = insertWith flip flagName (Enabled True)

flip :: Value -> Value -> Value
flip _ (Enabled value) = Enabled $ not value

setEnabledBool :: String -> ComptonMap -> ComptonMap
setEnabledBool flagName = insert flagName $ Enabled True

unsetEnabledBool :: String -> ComptonMap -> ComptonMap
unsetEnabledBool flagName = insert flagName $ Enabled False
