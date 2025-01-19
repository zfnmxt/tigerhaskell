module Parser where

import AST
import Control.Applicative ((<*))
import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    SourcePos,
    anySingle,
    choice,
    chunk,
    empty,
    eof,
    errorBundlePretty,
    getSourcePos,
    many,
    notFollowedBy,
    option,
    parse,
    satisfy,
    sepBy1,
    some,
    try,
    (<|>),
  )
import Text.Megaparsec.Byte.Lexer qualified as L
import Text.Megaparsec.Char (space, space1)

type Parser = Parsec Void String

spaceConsumer :: Parser ()
spaceConsumer =
  L.space
    space1
    empty
    skipBlockComments
  where
    skipBlockComments = void $ do
      symbol "/*"
      skipBlockComments <|> void anySingle <|> empty
      symbol "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer
  where

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

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

lId :: Parser String
lId = lexeme $ try $ do
  c <- satisfy isAlpha
  cs <- many $ satisfy $ \c' -> isAlphaNum c' || c' == '_'
  pure $ c : cs

pTyId :: Parser (String, SourcePos)
pTyId = (,) <$> lId <*> getSourcePos

pTyAnnot :: Parser (Maybe (String, SourcePos))
pTyAnnot = option Nothing $ Just <$> (symbol ":" >> pTyId)

pField :: Parser Field
pField = Field <$> lId <* symbol ":" <*> lId <*> getSourcePos

pFields :: Parser [Field]
pFields = sepBy1 pField (symbol ",")

pDec :: Parser Dec
pDec = (FunctionDec <$> pFunDec) <|> pVarDec <|> pTypeDec
  where
    pVarDec = do
      symbol "var"
      id <- lId
      type_id <- pTyAnnot
      symbol ":="
      exp <- pExp
      VarDec id type_id exp <$> getSourcePos

    pTypeDec = do
      symbol "type"
      type_id <- lId
      symbol "="
      ty <- pTy
      pos <- getSourcePos
      pure $ TypeDec (type_id, ty, pos)

pFunDec :: Parser FunDec
pFunDec = do
  symbol "function"
  FunDec <$> lId <*> pFields <*> pTyAnnot <*> pExp <*> getSourcePos

pTy :: Parser Ty
pTy = undefined

pExp :: Parser Exp
pExp = undefined

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
