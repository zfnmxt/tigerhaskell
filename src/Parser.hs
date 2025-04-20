module Parser (parse) where

import AST
import Control.Monad (void, when)
import Data.Char (chr, isAlpha, isAlphaNum, ord)
import Text.Megaparsec
  ( Parsec,
    ShowErrorComponent (..),
    anySingle,
    between,
    choice,
    customFailure,
    empty,
    eof,
    errorBundlePretty,
    getSourcePos,
    many,
    notFollowedBy,
    option,
    optional,
    satisfy,
    sepBy,
    sepBy1,
    some,
    try,
    (<|>),
  )
import Text.Megaparsec qualified
import Text.Megaparsec.Byte.Lexer qualified as L
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    digitChar,
    printChar,
    space,
    space1,
  )
import Text.Megaparsec.Pos
  ( SourcePos (..),
    mkPos,
  )

type Parser = Parsec Error String

data Error
  = NotKeyword String
  | InvalidASCII Integer
  deriving (Eq, Show, Ord)

instance ShowErrorComponent Error where
  showErrorComponent (NotKeyword s) = "not a keyword: " <> s
  showErrorComponent (InvalidASCII i) = "invalid ASCII character code: " <> show i

parse :: FilePath -> String -> Either String [Dec]
parse fname s =
  case Text.Megaparsec.parse (space *> pDecs <* eof) fname s of
    Left err -> Left $ errorBundlePretty err
    Right x -> Right x

noSrcPos :: SourcePos
noSrcPos = SourcePos "<no location>" (mkPos 1) (mkPos 1)

withSrcPos :: Parser (SourcePos -> a) -> Parser a
withSrcPos p = do
  pos <- getSourcePos
  ($ pos) <$> p

pChainL :: forall a. (Parser (a -> a -> a)) -> Parser a -> Parser a
pChainL pOp p = do
  e <- p
  rest <- pRest
  pure $ rest e
  where
    pRest :: Parser (a -> a)
    pRest =
      choice
        [ do
            op <- pOp
            r <- p
            rest <- pRest
            pure $ \l -> rest $ l `op` r,
          pure id
        ]

pChainR :: forall a. (Parser (a -> a -> a)) -> Parser a -> Parser a
pChainR pOp p = do
  e <- p
  rest <- pRest
  pure $ rest e
  where
    pRest :: Parser (a -> a)
    pRest =
      choice
        [ do
            op <- pOp
            r <- p
            rest <- pRest
            pure $ \l -> l `op` rest r,
          pure id
        ]

integerLit :: Integer -> Exp
integerLit i = IntExp i noSrcPos

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
      symbol_ "/*"
      skipBlockComments <|> void anySingle <|> empty
      symbol_ "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer
  where

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

symbol_ :: String -> Parser ()
symbol_ = void . symbol

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
pTyId = withSrcPos $ (,) <$> lId

pTyAnnot :: Parser (Maybe (String, SourcePos))
pTyAnnot = option Nothing $ Just <$> (symbol ":" >> pTyId)

pField :: Parser Field
pField = withSrcPos $ Field <$> lId <* symbol ":" <*> lId

pDecs :: Parser [Dec]
pDecs = many pDec

pDec :: Parser Dec
pDec =
  choice
    [ FunctionDec <$> pFunDec,
      pVarDec,
      pTypeDec
    ]
  where
    pVarDec = withSrcPos $ do
      pKeyword "var"
      var_id <- lId
      type_id <- pTyAnnot
      symbol_ ":="
      e <- pExp
      pure $ VarDec var_id type_id e

    pTypeDec = withSrcPos $ do
      pKeyword "type"
      type_id <- lId
      symbol_ "="
      ty <- pTy
      pure $ \pos -> TypeDec (type_id, ty, pos)

pFunDec :: Parser FunDec
pFunDec = withSrcPos $ do
  pKeyword "function"
  FunDec <$> lId <*> pFields <*> pTyAnnot <*> pExp
  where
    pFields = sepBy1 pField (symbol ",")

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
          (symbol_ "{")
          (symbol_ "}")
          ( let pTyField = do
                  field_id <- lId
                  symbol_ ":"
                  (type_id, pos) <- pTyId
                  pure $ Field field_id type_id pos
             in pTyField `sepBy1` symbol_ ","
          )
    pArrayTy =
      uncurry ArrayTy <$> (pKeyword "array" *> pKeyword "of" *> pTyId)

pVar :: Parser Var
pVar = do
  pos <- getSourcePos
  v <- SimpleVar <$> lId <*> pure pos
  rest <- pAccess
  pure $ rest v
  where
    pAccess =
      choice
        [ withSrcPos $ do
            symbol_ "."
            field <- lId
            rest <- pAccess
            pure $ \pos v -> rest $ FieldVar v field pos,
          withSrcPos $ do
            e <- between (symbol_ "[") (symbol_ "]") pExp
            rest <- pAccess
            pure $ \pos v -> rest $ SubscriptVar v e pos,
          pure id
        ]

pExp :: Parser Exp
pExp = pOr
  where
    pOr :: Parser Exp
    pOr =
      flip pChainR pAnd $ withSrcPos $ do
        symbol_ "|"
        pure $ \pos l r -> IfExp l (integerLit 1) (Just r) pos

    pAnd :: Parser Exp
    pAnd =
      flip pChainR pCmp $ withSrcPos $ do
        symbol_ "&"
        pure $ \pos l r -> IfExp l r (Just $ integerLit 0) pos

    pCmp :: Parser Exp
    pCmp = pOpExp (pOper opMap) pPlusMinus
      where
        opMap =
          [ ("=", EqOp),
            ("<>", NeqOp),
            ("<", LtOp),
            ("<=", LeOp),
            (">", GtOp),
            (">=", GeOp)
          ]

    pPlusMinus :: Parser Exp
    pPlusMinus = pOpExp (pOper [("+", PlusOp), ("-", MinusOp)]) pTimesDiv

    pTimesDiv :: Parser Exp
    pTimesDiv = pOpExp (pOper [("*", TimesOp), ("/", DivideOp)]) pAtom

    pOper :: [(String, Oper)] -> Parser Oper
    pOper =
      choice
        . map (\(s, op) -> symbol s *> pure op)

    pOpExp :: Parser Oper -> Parser Exp -> Parser Exp
    pOpExp pOp = pChainL $ withSrcPos $ do
      op <- pOp
      pure $ \pos l r -> OpExp l op r pos

pAtom :: Parser Exp
pAtom = do
  choice
    [ pNegate,
      VarExp <$> pVar,
      pKeyword "nil" *> pure NilExp,
      withSrcPos $ IntExp <$> lInteger,
      withSrcPos $ StringExp <$> lString,
      withSrcPos $ CallExp <$> lId <*> pExp `sepBy` symbol_ ",",
      pRecord,
      SeqExp
        <$> between
          (symbol_ "(")
          (symbol_ ")")
          (withSrcPos ((,) <$> pExp) `sepBy1` symbol_ ";"),
      withSrcPos $ AssignExp <$> pVar <*> pExp,
      pIf,
      pWhile,
      pFor,
      withSrcPos $ pKeyword "break" *> pure BreakExp,
      pLet,
      pArray
    ]
  where
    pNegate =
      withSrcPos $
        OpExp (integerLit 0) MinusOp <$> (symbol_ "-" *> pAtom)
    pRecord =
      let pFields =
            between (symbol_ "{") (symbol_ "}") $
              (withSrcPos $ (,,) <$> lId <*> (symbol_ "=" *> pExp)) `sepBy` symbol_ ","
       in withSrcPos $
            RecordExp <$> pFields <*> lId
    pIf = do
      withSrcPos $
        IfExp
          <$> (pKeyword "if" *> pExp)
          <*> (pKeyword "then" *> pExp)
          <*> (optional $ pKeyword "else" *> pExp)
    pWhile =
      withSrcPos $
        WhileExp
          <$> (pKeyword "while" *> pExp)
          <*> (pKeyword "do" *> pExp)

    pFor =
      withSrcPos $
        ForExp
          <$> lId
          <*> (symbol_ ":=" *> pExp)
          <*> (pKeyword "to" *> pExp)
          <*> (pKeyword "do" *> pExp)

    pLet =
      withSrcPos $
        LetExp
          <$> (pKeyword "let" *> many pDec)
          <*> (pKeyword "in" *> pExp <* pKeyword "end")

    pArray =
      withSrcPos $
        ArrayExp
          <$> lId
          <*> (between (symbol_ "[") (symbol_ "]") pExp)
          <*> (pKeyword "of" *> pExp)
