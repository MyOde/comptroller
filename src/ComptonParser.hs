{-# LANGUAGE RecordWildCards #-}
module ComptonParser
  ( parseComptonFile
  , testParser
  , test2
  ) where

-- TODO Remove - used for tests. Create tests
import           System.IO
import           Text.Parsec                   (ParseError, endBy, sepBy,
                                                skipMany, try)
import           Text.Parsec.Char              (char, digit, noneOf, oneOf,
                                                satisfy, spaces, string)
import           Text.Parsec.Combinator        (option)
import           Text.ParserCombinators.Parsec (Parser, many1, parseFromFile,
                                                (<|>))

type Entry = (String, Value)
-- TODO Remember to remove all the show derivations - they're not
-- needed beyond debugging purposes
data OpacityValue = OpacityValue { opacity    :: Integer
                                 , selector   :: Selector
                                 , comparison :: Comparer
                                 , value      :: String
                                 } deriving Show

data RegularValue = RegularValue { r_selector   :: Selector
                                 , r_comparison :: Comparer
                                 , r_value      :: String
                                 } deriving Show

data WinType = WinType { name      :: String
                       , arguments :: [WinTypeArg]
                       } deriving Show

data Selector = Class | Name | WindowType deriving (Enum, Show)
data Comparer = Equal | EqualInsens | Like | LikeInsens deriving (Enum, Show)
-- TODO: Should find out how to limit what I allow from The Value data type
data WinTypeArg = Fade Value | Shadow Value | Opacity Value | Focus Value deriving Show

data Value
  = Enabled Bool
  | Textual String
  | Numeric Integer
  | Floating Double
  | WinTypes [WinType]
  | OpacityRules [OpacityValue]
  | RegularRules [RegularValue] deriving Show

opacityRuleName = "opacity-rule"

parseComptonFile :: String -> IO (Either ParseError [Entry])
parseComptonFile filePath = parseFromFile comptonConfigParser filePath

comptonConfigParser :: Parser [Entry]
comptonConfigParser = endBy entryGrouper (char ';' <* whitespace)

whitespace :: Parser ()
whitespace = skipMany $ oneOf " \t\n"

entryGrouper :: Parser Entry
entryGrouper = opacityRule <|> genericEntry

opacityRule :: Parser Entry
opacityRule = (,) <$> name <*> opacities
  where name = try $ string opacityRuleName <* whitespace <* char '='
        opacities = whitespace *>  char '[' *> pOpacityArray <* char ']'

genericEntry :: Parser Entry
genericEntry = (,)
  <$> many1 (noneOf " =:")
  <*> (spaces *> (parseEntry <|> parseWinTypesReal))

parseWindowTypeOption :: String -> Parser Value -> Parser Value
parseWindowTypeOption name valueParser =
  ((try $ string name) *> whitespace *> char '='
   *> whitespace *> valueParser <* char ';'
      )

win_Fade = Fade <$> parseWindowTypeOption "fade" pEnabled
win_Shadow = Shadow <$> parseWindowTypeOption "shadow" pEnabled
win_Opacity = Opacity <$> parseWindowTypeOption "opacity" pFloater
win_Focus = Focus <$> parseWindowTypeOption "focus" pEnabled

parseWinType :: Parser [WinTypeArg]
parseWinType = whitespace *> many1 (argumentParser <* whitespace)
  where argumentParser = win_Shadow <|> win_Opacity <|> win_Fade <|> win_Focus

parseWinTypes :: Parser WinType
parseWinTypes = WinType <$> name <*> winTypes
  where name = many1 (noneOf " :\t{}")
        winTypes = (whitespace *> char ':' *> whitespace *> char '{' *> parseWinType <* whitespace <* char '}' <* char ';' <* whitespace)

parseWinTypesReal :: Parser Value
parseWinTypesReal = WinTypes <$>
  (char ':' *> whitespace *> char '{' *> whitespace
  *> (many1 parseWinTypes)
  <* whitespace <* char '}'
  )

parseEntry :: Parser Value
parseEntry = char '=' *> spaces *> content
  where content = pArray <|> pEnabled <|> pFloater
            <|> pInteger <|> pNatural <|> pTextual

-- TODO I really like the trip that I had while naming these things
pTextual :: Parser Value
pTextual = fmap Textual
  $ spaces *> char '"' *> many1 (noneOf "\"") <* char '"'

pArray :: Parser Value
pArray = char '[' *> spaces *> pRegularArray <* char ']'

pRegularArray :: Parser Value
pRegularArray = fmap RegularRules
  $ (sepBy pRegularLine $ char ',') <* whitespace

pOpacityArray :: Parser Value
pOpacityArray = fmap OpacityRules
  $ (sepBy pOpacityLine $ char ',') <* whitespace

opaFromRegular :: Integer -> RegularValue -> OpacityValue
opaFromRegular opacity RegularValue{..} =
  OpacityValue opacity r_selector r_comparison r_value

pOpacityLine :: Parser OpacityValue
pOpacityLine = opaFromRegular <$> (read <$> pOpa) <*> pVal
  where pOpa = whitespace *> char '"' *> try naturalNumber
        pVal = char ':' *> regularLineBase <* char '"'

regularLineBase :: Parser RegularValue
regularLineBase = RegularValue <$> parseSelector <*> comp <*> value
  where comp = whitespace *> stringComparer <* whitespace
        value = char '\'' *> many1 (noneOf "'") <* char '\''

pRegularLine :: Parser RegularValue
pRegularLine = spaces *> char '"' *> regularLineBase <* char '"' <* spaces

pOpacityNumber :: Parser String
pOpacityNumber = char '"' *> naturalNumber <* char ':'

naturalNumber :: Parser String
naturalNumber = many1 digit
pMinusInteger :: Parser String
pMinusInteger = (:) <$> char '-' <*> naturalNumber
pInt :: Parser String
pInt = pMinusInteger <|> naturalNumber

pFloat :: Parser Double
pFloat = fmap rd $ (++) <$> pInt <*> decimal
  where rd      = read :: String -> Double
        decimal = option "" $ (:) <$> char '.' <*> naturalNumber

-- TODO Find how to simplify this shi*. Pretty sure I'll just rewrite the
-- dependent stuff to not need something weird like this
tryAndClassify :: String -> a -> Parser a
tryAndClassify tryWord successReturn = do
  try $ string tryWord
  return successReturn

tryString :: String -> Parser String
tryString word = try $ string word

pNatural :: Parser Value
pNatural = fmap Numeric $ read <$> try naturalNumber
pInteger :: Parser Value
pInteger = fmap Numeric $ read <$> try pInt
pFloater :: Parser Value
pFloater = Floating <$> try pFloat
pEnabled :: Parser Value
pEnabled = Enabled <$> parseBoolean

stringComparer :: Parser Comparer
stringComparer = pEqualComp <|> pLikeComp <|> pEqualInsens <|> pLikeInsens
  where pEqualComp = Equal <$ tryString "="
        pLikeComp = Like <$ tryString "*="
        pLikeInsens = LikeInsens <$ tryString "*?="
        pEqualInsens = EqualInsens <$ tryString "?="

parseSelector :: Parser Selector
parseSelector = pName <|> pClass <|> pWindowType
  where pName = Name <$ tryString "name"
        pClass = Class <$ tryString "class_g"
        pWindowType = WindowType <$ tryString "window_type"

parseBoolean :: Parser Bool
parseBoolean = pTrue <|> pFalse
  where pTrue = True <$ tryString "true"
        pFalse = False <$ tryString "false"

-- ++++++++++++++++++++
-- ADHOC TEST FUNCTIONS
-- ++++++++++++++++++++
log_file_path = "/home/bmiww/comproller_log_parsed2"
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
