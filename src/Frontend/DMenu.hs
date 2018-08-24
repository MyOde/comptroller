{-# LANGUAGE FlexibleContexts #-}

module Frontend.DMenu where

import           Control.Monad.IO.Class (liftIO)
import           DMenu                  (MonadDMenu, ProcessError, numLines,
                                         prompt, selectWith, (.=))

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
