module Parser where

import AST
import Control.Applicative ((<*))
import Control.Monad (guard, void, when)
import Data.Char (chr, isAlpha, isAlphaNum, isDigit, ord)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    ShowErrorComponent (..),
    SourcePos,
    anySingle,
    between,
    choice,
    chunk,
    customFailure,
    empty,
    eof,
    errorBundlePretty,
    getSourcePos,
    many,
    notFollowedBy,
    option,
    parse,
    parseError,
    satisfy,
    sepBy1,
    some,
    try,
    (<|>),
  )
import Text.Megaparsec.Byte.Lexer qualified as L
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    digitChar,
    printChar,
    space,
    space1,
    string,
  )

type Parser = Parsec Error String

data Error
  = NotKeyword String
  | InvalidASCII Integer
  deriving (Eq, Show, Ord)

instance ShowErrorComponent Error where
  showErrorComponent (NotKeyword s) = "not a keyword: " <> s
  showErrorComponent (InvalidASCII i) = "invalid ASCII character code: " <> show i

lId :: Parser String
lId = lexeme $ try $ do
  c <- satisfy isAlpha
  cs <- many $ satisfy $ \c' -> isAlphaNum c' || c' == '_'
  pure $ c : cs

lInteger :: Parser Integer
lInteger =
  lexeme $ read <$> some digitChar <* notFollowedBy alphaNumChar

lString :: Parser String
lString =
  lexeme $ char '"' *> munch
  where
    munch =
      choice
        [ try format *> munch,
          do
            next <- printChar
            case next of
              '"' -> pure mempty
              '\\' -> (:) <$> escape <*> munch
              _ -> (next :) <$> munch
        ]
    escape =
      choice
        [ char 'n' *> pure '\n',
          char 't' *> pure '\t',
          char '^' *> control,
          ascii,
          char '"',
          char '\\'
        ]
    control =
      chr . (flip (-) (ord 'A')) . ord
        <$> satisfy (`elem` ['A' .. 'Z'])
    ascii = do
      ddd <- many digitChar
      when (length ddd /= 3) $
        customFailure $
          InvalidASCII $
            read ddd
      pure $ chr $ read ddd
    format = between (char '\\') (char '\\') space

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

pKeyword :: String -> Parser ()
pKeyword s
  | s `elem` keywords =
      void $ try (symbol s) <* notFollowedBy (satisfy isAlphaNum)
  | otherwise = customFailure $ NotKeyword s

pTyId :: Parser (String, SourcePos)
pTyId = do
  pos <- getSourcePos
  (,) <$> lId <*> pure pos

pTyAnnot :: Parser (Maybe (String, SourcePos))
pTyAnnot = option Nothing $ Just <$> (symbol ":" >> pTyId)

pField :: Parser Field
pField = do
  pos <- getSourcePos
  Field <$> lId <* symbol ":" <*> lId <*> pure pos

pFields :: Parser [Field]
pFields = sepBy1 pField (symbol ",")

pDec :: Parser Dec
pDec =
  choice
    [ FunctionDec <$> pFunDec,
      pVarDec,
      pTypeDec
    ]
  where
    pVarDec = do
      pos <- getSourcePos
      pKeyword "var"
      id <- lId
      type_id <- pTyAnnot
      symbol ":="
      exp <- pExp
      pure $ VarDec id type_id exp pos

    pTypeDec = do
      pos <- getSourcePos
      pKeyword "type"
      type_id <- lId
      symbol "="
      ty <- pTy
      pure $ TypeDec (type_id, ty, pos)

pFunDec :: Parser FunDec
pFunDec = do
  pKeyword "function"
  pos <- getSourcePos
  FunDec <$> lId <*> pFields <*> pTyAnnot <*> pExp <*> pure pos

pTy :: Parser Ty
pTy =
  choice
    [ pNameTy,
      pRecordTy,
      pArrayTy
    ]
  where
    pNameTy = uncurry NameTy <$> pTyId
    pRecordTy =
      RecordTy
        <$> between
          (symbol "{")
          (symbol "}")
          ( let pTyField = do
                  id <- lId
                  symbol ":"
                  (type_id, pos) <- pTyId
                  pure $ Field id type_id pos
             in pTyField `sepBy1` symbol ","
          )
    pArrayTy =
      uncurry ArrayTy <$> (pKeyword "array" *> pKeyword "of" *> pTyId)

pExp :: Parser Exp
pExp = do
  pos <- getSourcePos
  choice
    [ VarExp <$> pVar,
      pKeyword "nil" *> pure NilExp,
      IntExp <$> lInteger <*> pure pos,
      StringExp <$> lString <*> pure pos
    ]

pVar :: Parser Var
pVar = do
  pos <- getSourcePos
  v <- SimpleVar <$> lId <*> pure pos
  rest <- pAccess
  pure $ rest v
  where
    pAccess =
      choice
        [ do
            pos <- getSourcePos
            symbol "."
            field <- lId
            rest <- pAccess
            pure $ \v -> rest $ FieldVar v field pos,
          do
            pos <- getSourcePos
            e <- between (symbol "[") (symbol "]") pExp
            rest <- pAccess
            pure $ \v -> rest $ SubscriptVar v e pos,
          pure id
        ]
