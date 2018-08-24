module Frontend.Terminal where

import           Data.List

mapInd :: Int -> (a -> Int -> b) -> [a] -> [b]
mapInd startNumber f l = zipWith f l [startNumber..]

mapInd1 = mapInd 1

launchSelect :: [(String, b)] -> IO b
launchSelect options = do
  foldr1 (>>) $ nextPhase
  promptUntilCorrect indexed
    where indexed = (mapInd1 addIndexNumber options)
          nextPhase = map (putStrLn . prettyIndexed) indexed

promptUntilCorrect :: [(String, Int, b)] -> IO b
promptUntilCorrect options =
  promptForChoice
       >>= (\selection ->
            case find (\(_, index, _) -> show index == selection) options
  of Just (_, _, result) -> return result
     Nothing             -> promptUntilCorrect options)

promptForChoice :: IO String
promptForChoice = do
  putStrLn "Choose a number representing an option"
  getLine

prettyIndexed :: (String, Int, b) -> String
prettyIndexed (name, index, _) = show index ++ ":" ++ name

addIndexNumber :: (String, b) -> Int -> (String, Int, b)
addIndexNumber (name, element) index = (name, index, element)
