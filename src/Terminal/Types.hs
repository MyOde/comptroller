module Terminal.Types where

data EqualityMatcher
  = EqualMatch
  | PartialMatch
data SensitivityMatcher
  = SensitiveMatch
  | InsensitiveMatch
data IdentifyBy
  = ClassName
  | WindowName
