{-# LANGUAGE RecordWildCards #-}
module Compton.Parser
  ( readComptonFile
  ) where


import           Compton.Static
import           Compton.Types
import           Data.List                     (foldr1)
import           Data.Map.Strict               (fromList)
import           Data.Text                     (unpack)
import           Data.Text.IO                  (readFile)
import           Prelude                       hiding (readFile)
import           Text.Parsec                   (ParseError, endBy, sepEndBy,
                                                skipMany, try)
import           Text.Parsec.Char              (char, digit, noneOf, oneOf,
                                                string)
import           Text.Parsec.Combinator        (option)
import           Text.ParserCombinators.Parsec (Parser, many1, parse, (<|>))


readComptonFile :: String -> IO ComptonMap
readComptonFile configPath = parseComptonFile
  . unpack
  <$> readFile configPath

staticNameParser :: [String] -> Parser Value -> Parser Entry
staticNameParser entryNames valueParser =
  foldr1 (<|>) (map (parseEntry valueParser) entryNames)

parseBooleanEntry :: Parser Entry
parseBooleanEntry = staticNameParser booleanEntries pEnabled
parseFloatingEntry :: Parser Entry
parseFloatingEntry = staticNameParser floatingEntries pFloater
parseArrayEntry :: Parser Entry
parseArrayEntry = staticNameParser arrayEntries pArray
parseTextEntry :: Parser Entry
parseTextEntry = staticNameParser textualEntries pTextual

parseOpacityRuleEntry :: Parser Entry
parseOpacityRuleEntry = (,) <$> name <*> opacities
  where name = try $ string c_opacityRule <* whitespace <* char '='
        opacities = whitespace *> char '[' *> pOpacityArray <* char ']'

parseWinTypesEntry :: Parser Entry
parseWinTypesEntry = (,) <$> pWinTypesName <*> parseWinTypes
  where pWinTypesName = try $ string c_wintypes <* whitespace

parseAnyEntry :: Parser Entry
parseAnyEntry = parseBooleanEntry
                <|> parseFloatingEntry
                <|> parseArrayEntry
                <|> parseTextEntry
                <|> parseOpacityRuleEntry
                <|> parseWinTypesEntry

parseComptonFile :: String -> ComptonMap
parseComptonFile fileContents = case parse comptonConfigParser "(unknown)" fileContents of
  Left errorMessage -> error
                      $ "Failed parsing compton configuration file:\n"
                      ++ show errorMessage
  Right result      -> result

comptonConfigParser :: Parser ComptonMap
comptonConfigParser = fromList <$> endBy parseAnyEntry (char ';' <* whitespace)

whitespace :: Parser ()
whitespace = skipMany $ oneOf " \t\n"

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

parseWinTypes' :: Parser WinType
parseWinTypes' = WinType <$> name <*> winTypes
  where name = many1 (noneOf " :\t{}")
        winTypes = (whitespace *> char ':' *> whitespace *> char '{' *> parseWinType <* whitespace <* char '}' <* char ';' <* whitespace)

parseWinTypes :: Parser Value
parseWinTypes = WinTypes <$>
  (char ':' *> whitespace *> char '{' *> whitespace
  *> (many1 parseWinTypes')
  <* whitespace <* char '}'
  )

parseEntry :: Parser Value -> String -> Parser Entry
parseEntry parseValue name = (,) <$> parseEntryName <*> parseValue
  where parseEntryName = try $ string name <* whitespace <* char '=' <* whitespace

-- TODO I really like the trip that I had while naming these things
pTextual :: Parser Value
pTextual = fmap Textual
  $ whitespace *> char '"' *> many1 (noneOf "\"") <* char '"'

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
