module XpropParser
  ( parseXpropOutput
  ) where

import           Control.Monad                      (void)
import           Data.Functor.Identity              (Identity)
import           DataTypes                          (Props (..))
import           Text.Parsec.Char                   (char, noneOf, spaces)
import           Text.Parsec.Prim                   (ParsecT)
import           Text.ParserCombinators.Parsec      (many1, parse)
import           Text.ParserCombinators.Parsec.Prim (skipMany)

parseXpropOutput :: String -> String
parseXpropOutput text = case parse lineParse "(unknown)" text of
  Left errorMessage -> error "oh nopes"
  Right result      -> result

lineParse :: ParsecT String u Identity [Char]
lineParse = do
  skipMany (noneOf "=")
  void $ char '='
  spaces
  void $ char '"'
  many1 (noneOf "\"")
