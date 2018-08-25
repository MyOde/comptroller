module Xprops.Parser
  ( parseXpropOutput
  ) where

import           Control.Monad                 (void)
import           Text.Parsec.Char              (char, noneOf, spaces)
import           Text.ParserCombinators.Parsec (Parser, many1, parse, skipMany,
                                                (<|>))

parseXpropOutput :: String -> String
parseXpropOutput text = case parse lineParse "(unknown)" text of
  Left errorMessage -> error $ "OH NOPES XPROP\n" ++ show errorMessage
  Right result      -> result

lineParse :: Parser String
lineParse =
  (skipMany $ noneOf " =")
  *> (spaces *> char '=' *> spaces *> (parseString <|> parseOther))

parseString :: Parser String
parseString = char '"' *> many1 (noneOf "\"") <* char '"'
parseOther :: Parser String
parseOther = many1 $ noneOf "|"

