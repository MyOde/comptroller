module XpropParser
  ( parseXpropOutput
  , getNameLine
  , getClassLine
  ) where

import           Control.Monad                 (void)
import           Text.Parsec.Char              (char, noneOf, spaces)
import           Text.ParserCombinators.Parsec (Parser, many1, parse, skipMany,
                                                (<|>))
import           Data.List         (find, lines)
import           Data.String.Utils (startswith)

class_identier = "WM_CLASS"
name_identifier = "WM_NAME"

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

getNameLine :: [String] -> String
getNameLine propsStrings = case find (startswith name_identifier) propsStrings of
  Just result -> result
  Nothing     -> error "Window missing the name property"

getClassLine :: [String] -> String
getClassLine propsString = case find (startswith class_identier) propsString of
  Just result -> result
  Nothing     -> error "Window missing the class property"
