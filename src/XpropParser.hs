module XpropParser
  ( parseXpropOutput
  ) where

import           Control.Monad                 (void)
import           Text.Parsec.Char              (char, noneOf, spaces)
import           Text.ParserCombinators.Parsec (Parser, many1, parse, skipMany)

parseXpropOutput :: String -> String
parseXpropOutput text = case parse lineParse "(unknown)" text of
  Left errorMessage -> error "oh nopes"
  Right result      -> result

lineParse :: Parser [Char]
lineParse =
  (skipMany $ noneOf "=")
  *> char '=' *> spaces *> char '"'
  *> many1 (noneOf "\"")
