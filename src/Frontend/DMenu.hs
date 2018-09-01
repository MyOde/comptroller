{-# LANGUAGE FlexibleContexts #-}

module Frontend.DMenu
  ( userInterface
  ) where

import           Control.Monad.IO.Class (liftIO)
import           DMenu                  (MonadDMenu, ProcessError, numLines,
                                         prompt, select, selectWith, (.=))
import           Frontend.Types

userInterface :: Frontend b
userInterface = Frontend launchSelect launchInput

-- TODO Ignoring informative text for now:
-- Since its interfering with regular input.
launchInput :: String -> IO String
launchInput _ =
  select options []
  >>= takeRight
  -- >>= validate helpText

validate :: String -> String -> IO String
validate expected actual = if expected == actual then launchInput expected else return actual

takeRight :: Either ProcessError String -> IO String
takeRight (Right result)     = return result
takeRight (Left (_, errVal)) = undefined

launchSelect :: [(String, b)] -> IO b
launchSelect entries = extractResult =<< selectWith options getDisplayName entries

getDisplayName :: (String, b) -> String
getDisplayName (name, _)  = name

extractResult :: Either ProcessError (String, b) -> IO b
extractResult (Left (_, errVal))  = undefined
extractResult (Right (_, result)) = return result

options :: MonadDMenu m => m ()
options = do
  numLines .= 10
  prompt .= "run"
