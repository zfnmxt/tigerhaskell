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
    getParserState,
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
    string,
  )
import Text.Megaparsec.Pos
  ( SourcePos (..),
    mkPos,
  )

type Parser = Parsec Error String

data Error
  = NotKeyword String
  | InvalidASCII Integer
  | Keyword String
  deriving (Eq, Show, Ord)

instance ShowErrorComponent Error where
  showErrorComponent (NotKeyword s) = "not a keyword: " <> s
  showErrorComponent (InvalidASCII i) = "invalid ASCII character code: " <> show i
  showErrorComponent (Keyword s) = "unexpected keyword: " <> s

parse :: FilePath -> String -> Either String Exp
parse fname s =
  case Text.Megaparsec.parse (space *> pExp <* eof) fname s of
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
  let x = c : cs
  if x `elem` keywords
    then customFailure $ NotKeyword x
    else pure x

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

lKeyword :: String -> Parser ()
lKeyword s
  | s `elem` keywords =
      lexeme $ void $ try (string s) <* notFollowedBy (satisfy isAlphaNum)
  | otherwise = customFailure $ NotKeyword s

pTyId :: Parser (String, SourcePos)
pTyId = withSrcPos $ (,) <$> lId

pTyAnnot :: Parser (Maybe (String, SourcePos))
pTyAnnot = option Nothing $ Just <$> (symbol ":" >> pTyId)

pField :: Parser Field
pField = withSrcPos $ Field <$> lId <* symbol ":" <*> lId

pDec :: Parser Dec
pDec =
  choice
    [ FunctionDec <$> pFunDec,
      pVarDec,
      pTypeDec
    ]
  where
    pVarDec = withSrcPos $ do
      lKeyword "var"
      var_id <- lId
      type_id <- pTyAnnot
      symbol_ ":="
      e <- pExp
      pure $ VarDec var_id type_id e

    pTypeDec = withSrcPos $ do
      lKeyword "type"
      type_id <- lId
      symbol_ "="
      ty <- pTy
      pure $ \pos -> TypeDec (type_id, ty, pos)

pFunDec :: Parser FunDec
pFunDec = withSrcPos $ do
  lKeyword "function"
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
      uncurry ArrayTy <$> (lKeyword "array" *> lKeyword "of" *> pTyId)

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
      pArrayVarAssignCall,
      lKeyword "nil" *> pure NilExp,
      withSrcPos $ IntExp <$> lInteger,
      withSrcPos $ StringExp <$> lString,
      pRecord,
      SeqExp
        <$> between
          (symbol_ "(")
          (symbol_ ")")
          (withSrcPos ((,) <$> pExp) `sepBy1` symbol_ ";"),
      pIf,
      pWhile,
      pFor,
      withSrcPos $ lKeyword "break" *> pure BreakExp,
      pLet
    ]
  where
    pNegate =
      withSrcPos $
        OpExp (integerLit 0) MinusOp <$> (symbol_ "-" *> pAtom)

    pArrayVarAssignCall =
      withSrcPos $ do
        x <- lId
        choice
          [ pArrayVarAssign x,
            pCall x
          ]
      where
        pArrayVarAssign :: String -> Parser (SourcePos -> Exp)
        pArrayVarAssign x = do
          res <- pAccessArray x
          case res of
            Left array_e -> pure array_e
            Right f -> do
              let f' = f . SimpleVar x
              mExp <- optional pExp
              case mExp of
                Nothing -> pure $ VarExp . f'
                Just e -> pure $ \pos -> AssignExp (f' pos) e pos

        pCall :: String -> Parser (SourcePos -> Exp)
        pCall x = CallExp x <$> pExp `sepBy` symbol_ ","

        pAccessArray :: String -> Parser (Either (SourcePos -> Exp) (Var -> Var))
        pAccessArray x =
          choice
            [ do
                e <- between (symbol_ "[") (symbol_ "]") pExp
                choice
                  [ (Left . ArrayExp x e)
                      <$> (lKeyword "of" *> pExp),
                    withSrcPos $ do
                      rest <- pAccess
                      pure $ \pos -> Right $ \v -> rest $ SubscriptVar v e pos
                  ],
              Right <$> pAccess
            ]

        pAccess :: Parser (Var -> Var)
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

    pRecord =
      let pFields =
            between (symbol_ "{") (symbol_ "}") $
              (withSrcPos $ (,,) <$> lId <*> (symbol_ "=" *> pExp)) `sepBy` symbol_ ","
       in withSrcPos $
            RecordExp <$> pFields <*> lId
    pIf = do
      withSrcPos $
        IfExp
          <$> (lKeyword "if" *> pExp)
          <*> (lKeyword "then" *> pExp)
          <*> (optional $ lKeyword "else" *> pExp)
    pWhile =
      withSrcPos $
        WhileExp
          <$> (lKeyword "while" *> pExp)
          <*> (lKeyword "do" *> pExp)

    pFor =
      withSrcPos $
        ForExp
          <$> lId
          <*> (symbol_ ":=" *> pExp)
          <*> (lKeyword "to" *> pExp)
          <*> (lKeyword "do" *> pExp)

    pLet =
      withSrcPos $
        LetExp
          <$> (lKeyword "let" *> many pDec)
          <*> (lKeyword "in" *> pExp <* lKeyword "end")
