-- TODO Please rename this
module Frontend.Types where

data Frontend a = Frontend
  { choice :: [(String, a)] -> IO a
  , input  :: String -> IO String
  }
