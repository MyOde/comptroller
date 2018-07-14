{-# LANGUAGE RecordWildCards #-}
module ComptonParser
  ( parseComptonFile
  -- , testParser
  -- , test2
  ) where

import           ComptonTypes
-- TODO Remove - used for tests. Create tests
import           System.IO
import           Text.Parsec                   (ParseError, endBy, sepEndBy,
                                                skipMany, try)
import           Text.Parsec.Char              (char, digit, noneOf, oneOf,
                                                satisfy, spaces, string)
import           Text.Parsec.Combinator        (option)
import           Text.ParserCombinators.Parsec (Parser, many1, parse,
                                                parseFromFile, (<|>))

opacityRuleName = "opacity-rule"

parseComptonFile :: String -> [Entry]
parseComptonFile fileContents = case parse comptonConfigParser "(unknown)" fileContents of
  Left errorMessage -> error $ "Failed parsing compton configuration file:\n" ++ show errorMessage
  Right result      -> result

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
pArray = char '[' *> whitespace *> pRegularArray <* char ']'

comaEliminateTrailingSpace :: Parser Char
comaEliminateTrailingSpace = char ',' <* whitespace

pRegularArray :: Parser Value
pRegularArray = fmap RegularRules
  $ (sepEndBy pRegularLine $ comaEliminateTrailingSpace) <* whitespace

pOpacityArray :: Parser Value
pOpacityArray = fmap OpacityRules
  $ (sepEndBy pOpacityLine $ comaEliminateTrailingSpace) <* whitespace

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
pRegularLine = char '"' *> regularLineBase <* char '"' <* whitespace

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
  where pTrue = True <$ (tryString "true" <|> tryString "True")
        pFalse = False <$ (tryString "false" <|> tryString "False")

-- ++++++++++++++++++++
-- ADHOC TEST FUNCTIONS
-- ++++++++++++++++++++
-- log_file_path = "/home/bmiww/comproller_log_parsed2"
-- config_file_path = "/home/bmiww/.config/compton.conf_"

-- testParser :: IO ()
-- testParser = do
--   result <- (parseComptonFile config_file_path)
--   writeFile log_file_path (show result)
--   print (show result)

-- test2 :: IO ()
-- test2 = do
--   result <- (parseComptonFile config_file_path)
--   handle <- openFile log_file_path ReadMode
--   contents <- hGetContents handle
--   let matching = (show result) == contents
--   print matching
--   print result
