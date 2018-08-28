module Compton.Types where

import           Data.List (find)
import           Data.Map.Strict (Map)

-- TODO Remember to remove all the show derivations - they're not
-- needed beyond debugging purposes

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
  | Numeric Integer
  | Floating Double
  | WinTypes [WinType]
  | OpacityRules { opacityValues :: [OpacityValue] }
  | RegularRules [RegularValue] deriving (Show)

selectorString :: Selector -> String
selectorString Class      = "class_g"
selectorString Name       = "name"
selectorString WindowType = "window_type"

comparerString :: Comparer -> String
comparerString Equal       = "="
comparerString Like        = "*="
comparerString LikeInsens  = "*?="
comparerString EqualInsens = "?="
