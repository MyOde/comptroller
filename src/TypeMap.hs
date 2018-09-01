{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TypeMap where

import           Compton.Types        (Comparer (..), Selector (..))
import           Control.Monad.Reader (MonadIO, MonadReader, ReaderT, ask)
import           Terminal.Parser      (ConsoleArguments, configurationPath)
import           Terminal.Types       (EqualityMatcher (..), IdentifyBy (..),
                                       SensitivityMatcher (..))

newtype ConsReadT a = ConsReadT
  { runConsArgRead :: ReaderT ConsoleArguments IO a
  } deriving (Applicative, Monad, Functor, MonadReader ConsoleArguments, MonadIO)

askConfigPath :: ConsReadT String
askConfigPath = configurationPath <$> ask


windowIdentifierSelector :: IdentifyBy -> Selector
windowIdentifierSelector ClassName  = Class
windowIdentifierSelector WindowName = Name

nameMatchComparer :: EqualityMatcher -> SensitivityMatcher -> Comparer
nameMatchComparer PartialMatch SensitiveMatch   = Like
nameMatchComparer PartialMatch InsensitiveMatch = LikeInsens
nameMatchComparer EqualMatch SensitiveMatch     = Equal
nameMatchComparer EqualMatch InsensitiveMatch   = EqualInsens
