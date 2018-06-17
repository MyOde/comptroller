{-# LANGUAGE FlexibleContexts #-}

module Frontend where

-- import DMenu (run, numLines, prompt, selectM, MonadDMenu, (.=))
import           Control.Monad.IO.Class (liftIO)
import           DMenu

launch :: IO ()
-- launch = print =<< select options ["one", "two"]
launch = process =<< select options ["one", "two"]

-- process :: IO String -> IO ()
process :: Either ProcessError String -> IO ()
process selection = case selection of
  Left (_, errVal) -> error errVal
  Right "one"      -> launchOne
  Right "two"      -> launchTwo

launchOne = print =<< select options ["ONE-ANGA", "ONE-HUMBA"]
launchTwo = print =<< select options ["TWO-AMBA", "TWO-ZIMBA"]

options :: MonadDMenu m => m ()
options = do
  numLines .= 10
  prompt .= "run"

-- launch :: IO ()
-- launch = run $ do
--   numLines .= 10
--   prompt .= "run"
--   liftIO . print =<< selectM ["one", "two"]
