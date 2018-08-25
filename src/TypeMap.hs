module TypeMap where

import           Compton.Types  (Comparer (..), Selector (..))
import           Terminal.Types (EqualityMatcher (..), IdentifyBy (..),
                                 SensitivityMatcher (..))

windowIdentifierSelector :: IdentifyBy -> Selector
windowIdentifierSelector ClassName  = Class
windowIdentifierSelector WindowName = Name

nameMatchComparer :: EqualityMatcher -> SensitivityMatcher -> Comparer
nameMatchComparer PartialMatch SensitiveMatch   = Like
nameMatchComparer PartialMatch InsensitiveMatch = LikeInsens
nameMatchComparer EqualMatch SensitiveMatch     = Equal
nameMatchComparer EqualMatch InsensitiveMatch   = EqualInsens
