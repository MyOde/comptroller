{-# LANGUAGE RecordWildCards #-}
module ComptonParser
  ( parseComptonFile
  , testParser
  ) where

import           Control.Monad                      (void)
import           Data.Functor.Identity              (Identity)
import           DataTypes                          (ComptonConf (..))
import           Text.Parsec                        (ParseError, endBy,
                                                     optional, sepBy, skipMany,
                                                     try)
import           Text.Parsec.Char                   (alphaNum, char, digit,
                                                     noneOf, oneOf, satisfy,
                                                     spaces, string)
import           Text.Parsec.Combinator             (option)
import           Text.Parsec.Prim                   (ParsecT)
import           Text.Parsec.Token                  (float, integer, natural)
import           Text.ParserCombinators.Parsec      (many1, (<|>))
import           Text.ParserCombinators.Parsec.Prim (parseFromFile)

type Entry = (String, Value)
type Parser u a = ParsecT String u Identity a
data OpacityValue = OpacityValue { opacity    :: Integer
                                 -- TODO Enumerate possible selectors?
                                 , selector   :: Selector
                                 -- TODO Enumrate possible comparison fn?
                                 , comparison :: Comparer
                                 , value      :: String
                                 } deriving Show

data RegularValue = RegularValue { r_selector   :: Selector
                                 , r_comparison :: Comparer
                                 , r_value      :: String
                                 } deriving (Show)

data Selector = Class | Name | WindowType deriving (Enum, Show)
data Comparer = Equal | EqualInsens | Like | LikeInsens deriving (Enum, Show)

data Value
  = Enabled Bool
  | Textual String
  | Numeric Integer
  | Floating Double
  | OpacityRules [OpacityValue]
  | RegularRules [RegularValue] deriving Show

opacityRuleName = "opacity-rule"
parseComptonFile :: String -> IO (Either ParseError [Entry])
parseComptonFile filePath = parseFromFile comptonConfigParser filePath

whitespace = " \t\n"
comparisonStartSymbols = " =?*"
eol :: Parser u ()
eol = void $ char '\n'
noVoidEol :: Parser u Char
noVoidEol = char '\n'

closingBracketHack :: String -> Parser u String
closingBracketHack contents = do
  void $ char '}'
  return contents

specialCaseSemicolon :: Parser u Char
specialCaseSemicolon = do
  spaces
  charHit <- char ';'
  skipMany (oneOf whitespace)
  return charHit

comptonConfigParser :: Parser u [Entry]
comptonConfigParser = endBy entryGrouper specialCaseSemicolon

opacityRule :: Parser u Entry
opacityRule = do
  try $ string opacityRuleName
  spaces
  char '='
  spaces
  char '['
  array <- pOpacityArray
  char ']'
  return (opacityRuleName, array)

entryGrouper :: Parser u Entry
entryGrouper = opacityRule <|> genericEntry

genericEntry :: Parser u Entry
genericEntry = do
  name <- many1 (noneOf " =:")
  spaces
  -- contents <- parseEntry <|> parseObjectEntry
  contents <- parseEntry <|> parseObjectEntry
  return (name, contents)

parseSimpleValue :: Parser u String
parseSimpleValue = do
  skipMany (oneOf whitespace)
  name <- many1 (noneOf " =;{}")
  spaces
  void $ char '='
  spaces
  contents <- many1 (noneOf ";")
  void $ char ';'
  skipMany (oneOf whitespace)
  return (show (name, contents))

parseObjectNotation :: Parser u String
parseObjectNotation = do
  name <- many1 (noneOf " :\t{}")
  spaces
  void $ char ':'
  skipMany (oneOf whitespace)
  char '{'
  contents <- many1 parseSimpleValue
  skipMany (oneOf whitespace)
  char '}'
  char ';'
  skipMany (oneOf whitespace)
  return (show (name, contents))

-- TODO You cheated here
parseObjectEntry :: Parser u Value
parseObjectEntry = do
  void $ char ':'
  skipMany (oneOf whitespace)
  char '{'
  skipMany (oneOf whitespace)
  contents <- many1 parseObjectNotation
  skipMany (oneOf whitespace)
  char '}'
  return $ Textual (contents >>= id)

parseEntry :: Parser u Value
parseEntry = do
  void $ char '='
  spaces
  pArray <|> pTrue <|> pFalse <|> pFloater
    <|> pInteger <|> pNatural <|> pString

pString :: Parser u Value
pString = do
  spaces
  char '"'
  value <- many1 (noneOf "\"")
  char '"'
  return $ Textual value

pArray :: Parser u Value
pArray = do
  char '['
  spaces
  arrayVal <- pRegularArray
  -- skipMany (oneOf whitespace)
  char ']'
  return arrayVal

comaAndWhite :: Parser u Char
comaAndWhite = char ','

pRegularArray :: Parser u Value
pRegularArray = do
  entries <- sepBy pRegularLine comaAndWhite
  skipMany (oneOf whitespace)
  return (RegularRules entries)

pOpacityArray :: Parser u Value
pOpacityArray = do
  opacities <- sepBy pOpacityLine comaAndWhite
  skipMany (oneOf whitespace)
  return (OpacityRules opacities)

opaFromRegular :: Integer -> RegularValue -> OpacityValue
opaFromRegular opacity RegularValue{..} = OpacityValue opacity r_selector r_comparison r_value

pOpacityLine :: Parser u OpacityValue
pOpacityLine = do
  skipMany (oneOf whitespace)
  char '"'
  opacity <- try naturalNumber
  char ':'
  regVal <- regularLineBase
  char '"'
  return $ opaFromRegular (read opacity) regVal

regularLineBase :: Parser u RegularValue
regularLineBase = do
  selector <- pName <|> pClass <|> pWindowType
  spaces
  comparer <- stringComparer
  spaces
  char '\''
  value <- many1 (noneOf "\'")
  char '\''
  return (RegularValue selector comparer value)

pRegularLine :: Parser u RegularValue
pRegularLine = do
  spaces
  char '"'
  value <- regularLineBase
  char '"'
  spaces
  return value

pOpacityNumber = do
  char '"'
  value <- naturalNumber
  char ':'
  return value

naturalNumber :: Parser u String
naturalNumber = many1 digit
pMinusInteger :: Parser u String
pMinusInteger = (:) <$> char '-' <*> naturalNumber
pInt :: Parser u String
pInt = pMinusInteger <|> naturalNumber

pFloat :: Parser u Double
pFloat = fmap rd $ (++) <$> pInt <*> decimal
  where rd      = read :: String -> Double
        decimal = option "" $ (:) <$> char '.' <*> naturalNumber

-- TODO Not really much of a transformation. Also, find how to simplify this shi*
tryAndTransform :: String -> a -> Parser u a
tryAndTransform tryWord successReturn = do
  try (string tryWord)
  return successReturn

pNatural :: Parser u Value
pNatural = do
  number <- try naturalNumber
  return $ Numeric $ read number

pInteger :: Parser u Value
pInteger = do
  number <- (try pInt)
  return $ Numeric $ read number

pFloater :: Parser u Value
pFloater = do
  number <- try pFloat
  return $ Floating number

stringComparer :: Parser u Comparer
stringComparer = pEqualComp <|> pLikeComp <|> pEqualInsens <|> pLikeInsens
pEqualComp = tryAndTransform "=" Equal
pLikeComp = tryAndTransform "*=" Like
pLikeInsens = tryAndTransform "*?=" LikeInsens
pEqualInsens = tryAndTransform "?=" EqualInsens

pName :: Parser u Selector
pName = tryAndTransform "name" Name
pClass :: Parser u Selector
pClass = tryAndTransform "class_g" Class
pWindowType :: Parser u Selector
pWindowType = tryAndTransform "window_type" WindowType
pTrue :: Parser u Value
pTrue = tryAndTransform "true" (Enabled True)
pFalse :: Parser u Value
pFalse = tryAndTransform "false" (Enabled False)

-- ++++++++++++++++++++
-- ADHOC TEST FUNCTIONS
-- ++++++++++++++++++++
log_file_path = "/home/bmiww/comproller_log_parsed"
config_file_path = "/home/bmiww/.config/compton.conf_"

testParser :: IO ()
testParser = do
  result <- (parseComptonFile config_file_path)
  writeFile log_file_path (show result)
  print (show result)

addNewLine :: String -> String
addNewLine string = string ++ "\n"
