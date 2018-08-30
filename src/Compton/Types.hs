module Compton.Types where

import           Data.List (find)
import           Data.Map.Strict (Map)
import           Numeric         (showFFloat)

-- TODO Maybe should have introduced My own typeclass like - representable???
instance Show Value where
  show (Floating number)    = showFFloat Nothing number $ ""
  show (Textual string)     = show string
  show (Enabled bool)       = show bool
  show (WinTypes types)     = show types
  show (OpacityRules rules) = show rules
  show (RegularRules rules) = show rules

type Entry = (String, Value)
type ComptonMap = Map String Value

data OpacityValue = OpacityValue { opacity    :: Integer
                                 , selector   :: Selector
                                 , comparison :: Comparer
                                 , value      :: String
                                 } deriving Show

data RegularValue = RegularValue { r_selector   :: Selector
                                 , r_comparison :: Comparer
                                 , r_value      :: String
                                 } deriving Show

data WinType = WinType { name      :: String
                       , arguments :: [WinTypeArg]
                       } deriving Show

data Selector = Class | Name | WindowType deriving (Enum, Show)
data Comparer = Equal | EqualInsens | Like | LikeInsens deriving (Enum, Show)

-- TODO: Should find out how to limit what I allow from The Value data type
data WinTypeArg = Fade Value | Shadow Value | Opacity Value | Focus Value deriving Show
data Value
  = Enabled Bool
  | Textual String
  -- TODO maybe delete this numeric???
  | Numeric Integer
  | Floating Double
  | WinTypes [WinType]
  | OpacityRules { opacityValues :: [OpacityValue] }
  | RegularRules [RegularValue]

selectorString :: Selector -> String
selectorString Class      = "class_g"
selectorString Name       = "name"
selectorString WindowType = "window_type"

comparerString :: Comparer -> String
comparerString Equal       = "="
comparerString Like        = "*="
comparerString LikeInsens  = "*?="
comparerString EqualInsens = "?="
