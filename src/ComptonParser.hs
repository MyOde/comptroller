{-# LANGUAGE RecordWildCards #-}
module ComptonParser
  ( parseComptonFile
  , testParser
  , test2
  ) where

import           Control.Monad                      (void)
import           Data.Functor.Identity              (Identity)
import           DataTypes                          (ComptonConf (..))
-- TODO Remove - used for tests. Create tests
import           System.IO
import           Text.Parsec                        (ParseError, endBy, sepBy,
                                                     skipMany, try)
import           Text.Parsec.Char                   (char, digit, noneOf, oneOf,
                                                     satisfy, spaces, string)
import           Text.Parsec.Combinator             (option)
import           Text.Parsec.Prim                   (ParsecT)
import           Text.ParserCombinators.Parsec      (many1, (<|>))
import           Text.ParserCombinators.Parsec.Prim (parseFromFile)

type Entry = (String, Value)
type Parser u a = ParsecT String u Identity a
data OpacityValue = OpacityValue { opacity    :: Integer
                                 , selector   :: Selector
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

comptonConfigParser :: Parser u [Entry]
comptonConfigParser = endBy entryGrouper specialCaseSemicolon

whitespace :: Parser u ()
whitespace = skipMany $ oneOf " \t\n"

specialCaseSemicolon :: Parser u Char
specialCaseSemicolon = whitespace *> char ';' <* whitespace

opacityRule :: Parser u Entry
opacityRule = (,) opacityRuleName
  <$> (try $ string opacityRuleName
       *> spaces *> char '=' *> spaces *> char '['
       *> pOpacityArray <* char ']'
      )

entryGrouper :: Parser u Entry
entryGrouper = opacityRule <|> genericEntry

genericEntry :: Parser u Entry
genericEntry = (,) <$> many1 (noneOf " =:") <*> (spaces *> (parseEntry <|> parseObjectEntry))

parseSimpleValue :: Parser u String
parseSimpleValue = fmap show $ (,) <$> name <*> value where
  name = whitespace *> many1 (noneOf " =;{}") <* spaces <* char '=' <* spaces
  value = many1 (noneOf ";") <* char ';' <* whitespace

parseObjectNotation :: Parser u String
parseObjectNotation = fmap show $ (,) <$> name <*> value where
  name = many1 (noneOf " :\t{}") <* spaces <* char ':' <* whitespace <* char '{'
  value = many1 parseSimpleValue <* whitespace <* char '}' <* char ';' <* whitespace

-- TODO Still not fully parsing the JSON object notation
parseObjectEntry :: Parser e Value
parseObjectEntry = fmap (\cont -> Textual (cont >>= id))
  $ char ':' *> whitespace *> char '{' *> whitespace
  *> many1 parseObjectNotation
  <* whitespace <* char '}'

parseEntry :: Parser u Value
parseEntry = char '=' *> spaces *> content where
  content = pArray <|> pTrue <|> pFalse <|> pFloater
    <|> pInteger <|> pNatural <|> pString

pString :: Parser u Value
pString = fmap Textual
  $ spaces *> char '"' *> many1 (noneOf "\"") <* char '"'

pArray :: Parser u Value
pArray = char '[' *> spaces *> pRegularArray <* char ']'

pRegularArray :: Parser u Value
pRegularArray = fmap RegularRules
  $ (sepBy pRegularLine $ char ',') <* whitespace

pOpacityArray :: Parser u Value
pOpacityArray = fmap OpacityRules
  $ (sepBy pOpacityLine $ char ',') <* whitespace

opaFromRegular :: Integer -> RegularValue -> OpacityValue
opaFromRegular opacity RegularValue{..} =
  OpacityValue opacity r_selector r_comparison r_value

pOpacityLine :: Parser u OpacityValue
pOpacityLine = (\opa val -> (opaFromRegular (read opa) val)) <$> pOpa <*> pVal
  where pOpa = whitespace *> char '"' *> try naturalNumber
        pVal = char ':' *> regularLineBase <* char '"'

regularLineBase :: Parser u RegularValue
regularLineBase = RegularValue <$> parseSelector <*> comp <*> value
  where comp = spaces *> stringComparer <* spaces
        value = char '\'' *> many1 (noneOf "'") <* char '\''

pRegularLine :: Parser u RegularValue
pRegularLine = spaces *> char '"' *> regularLineBase <* char '"' <* spaces

pOpacityNumber :: Parser u String
pOpacityNumber = char '"' *> naturalNumber <* char ':'

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

-- TODO Find how to simplify this shi*
tryAndClassify :: String -> a -> Parser u a
tryAndClassify tryWord successReturn = do
  try $ string tryWord
  return successReturn

pNatural :: Parser u Value
pNatural = fmap Numeric $ read <$> try naturalNumber

pInteger :: Parser u Value
pInteger = fmap Numeric $ read <$> try pInt

pFloater :: Parser u Value
pFloater = Floating <$> try pFloat

stringComparer :: Parser u Comparer
stringComparer = pEqualComp <|> pLikeComp <|> pEqualInsens <|> pLikeInsens
pEqualComp = tryAndClassify "=" Equal
pLikeComp = tryAndClassify "*=" Like
pLikeInsens = tryAndClassify "*?=" LikeInsens
pEqualInsens = tryAndClassify "?=" EqualInsens

parseSelector :: Parser u Selector
parseSelector = pName <|> pClass <|> pWindowType
pName = tryAndClassify "name" Name
pClass = tryAndClassify "class_g" Class
pWindowType = tryAndClassify "window_type" WindowType

pBool :: Parser u Value
pBool = pTrue <|> pFalse
pTrue = tryAndClassify "true" (Enabled True)
pFalse = tryAndClassify "false" (Enabled False)

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

test2 :: IO ()
test2 = do
  result <- (parseComptonFile config_file_path)
  handle <- openFile log_file_path ReadMode
  contents <- hGetContents handle
  let matching = (show result) == contents
  print matching
  print result
