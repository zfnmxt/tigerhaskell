module Parser where

import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    choice,
    chunk,
    eof,
    errorBundlePretty,
    many,
    notFollowedBy,
    parse,
    satisfy,
    some,
    try,
  )
import Text.Megaparsec.Char (space)

type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme p = p <* space

keywords :: [String]
keywords =
  [ "while",
    "for",
    "to",
    "break",
    "let",
    "in",
    "end",
    "function",
    "var",
    "type",
    "array",
    "if",
    "then",
    "else",
    "do",
    "of",
    "nil"
  ]

-- lVName :: Parser VName
-- lVName = lexeme $ try $ do
--  c <- satisfy isAlpha
--  cs <- many $ satisfy isAlphaNum
--  let v = c : cs
--  if v `elem` keywords
--    then fail "Unexpected keyword"
--    else pure v
--
-- lInteger :: Parser Integer
-- lInteger =
--  lexeme $ read <$> some (satisfy isDigit) <* notFollowedBy (satisfy isAlphaNum)
--
-- lString :: String -> Parser ()
-- lString s = lexeme $ void $ chunk s
--
-- lKeyword :: String -> Parser ()
-- lKeyword s = lexeme $ void $ try $ chunk s <* notFollowedBy (satisfy isAlphaNum)
