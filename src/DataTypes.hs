module DataTypes
  ( Props(..)
  , ComptonConf(..)
  ) where

data Props = Props { className :: String
                   , name      :: String
                   }

data ComptonConf = ComptonConf { opacityRules :: [OpacityRule]
                               , mainOpacity :: Int
                               }

data OpacityRule = OpacityRule { value :: Int
                               , rule :: String
                               -- The hell were ==, *=, *?= these called?
                               , comparison :: Comparer
                               }

data Comparer = Equality | Like | LikeInvariant | EqualityInvariant
