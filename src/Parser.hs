module Parser (parse) where

import AST
import Control.Monad (void, when)
import Data.Char (chr, isAlpha, isAlphaNum, ord)
import Data.Maybe (isJust)
import Text.Megaparsec
  ( Parsec,
    ShowErrorComponent (..),
    between,
    choice,
    customFailure,
    empty,
    eof,
    errorBundlePretty,
    getSourcePos,
    many,
    manyTill,
    notFollowedBy,
    option,
    optional,
    satisfy,
    sepBy,
    sepBy1,
    some,
    try,
  )
import Text.Megaparsec qualified
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    digitChar,
    printChar,
    space,
    space1,
    string,
  )
import Text.Megaparsec.Char.Lexer qualified as L
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

parse :: FilePath -> String -> Either String UntypedExp
parse fname s =
  case Text.Megaparsec.parse (spaceConsumer *> pExp <* eof) fname s of
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

integerLit :: Integer -> UntypedExp
integerLit i = IntExp i noSrcPos

lId :: Parser String
lId = lexeme $ try $ do
  c <- satisfy isAlpha
  cs <- many $ satisfy $ \c' -> isAlphaNum c' || c' == '_'
  let x = c : cs
  if x `elem` keywords
    then customFailure $ Keyword x
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
    (L.skipBlockCommentNested "/*" "*/")

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

pField :: Parser UntypedField
pField = withSrcPos $ Field <$> lId <* symbol ":" <*> lId

pDec :: Parser UntypedDec
pDec =
  choice
    [ FunctionDec <$> some pFunDec,
      pVarDec,
      TypeDec <$> some pTypeDec
    ]
  where
    pVarDec = withSrcPos $ do
      lKeyword "var"
      var_id <- lId
      symbol_ ":"
      mtype_id <- option Nothing $ Just <$> pTyId
      when (isJust mtype_id) $ symbol_ ":"
      symbol_ "="
      e <- pExp
      pure $ VarDec var_id mtype_id e

    pTypeDec = withSrcPos $ do
      lKeyword "type"
      type_id <- lId
      symbol_ "="
      ty <- pTy
      pure $ \pos -> (type_id, ty, pos)

    pFunDec = withSrcPos $ do
      lKeyword "function"
      FunDec <$> lId <*> pFields <*> pTyAnnot <*> (symbol_ "=" *> pExp)
      where
        pFields = between (symbol_ "(") (symbol_ ")") $ sepBy pField (symbol ",")

pTy :: Parser UntypedTy
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

pExp :: Parser UntypedExp
pExp = pOr
  where
    pOr :: Parser UntypedExp
    pOr =
      flip pChainR pAnd $ withSrcPos $ do
        symbol_ "|"
        pure $ \pos l r -> IfExp l (integerLit 1) (Just r) pos

    pAnd :: Parser UntypedExp
    pAnd =
      flip pChainR pCmp $ withSrcPos $ do
        symbol_ "&"
        pure $ \pos l r -> IfExp l r (Just $ integerLit 0) pos

    pCmp :: Parser UntypedExp
    pCmp = pOpExp (pOper opMap) pPlusMinus
      where
        opMap =
          [ ("=", EqOp),
            ("<>", NeqOp),
            ("<=", LeOp),
            (">=", GeOp),
            ("<", LtOp),
            (">", GtOp)
          ]

    pPlusMinus :: Parser UntypedExp
    pPlusMinus = pOpExp (pOper [("+", PlusOp), ("-", MinusOp)]) pTimesDiv

    pTimesDiv :: Parser UntypedExp
    pTimesDiv = pOpExp (pOper [("*", TimesOp), ("/", DivideOp)]) pAtom

    pOper :: [(String, Oper)] -> Parser Oper
    pOper =
      choice
        . map (\(s, op) -> symbol_ s *> pure op)

    pOpExp :: Parser Oper -> Parser UntypedExp -> Parser UntypedExp
    pOpExp pOp = pChainL $ withSrcPos $ do
      op <- pOp
      pure $ \pos l r -> OpExp l op r pos

pAtom :: Parser UntypedExp
pAtom = do
  choice
    [ pNegate,
      pArrayVarAssignCallRecord,
      lKeyword "nil" *> pure NilExp,
      withSrcPos $ IntExp <$> lInteger,
      withSrcPos $ StringExp <$> lString,
      SeqExp
        <$> between
          (symbol_ "(")
          (symbol_ ")")
          (withSrcPos ((,) <$> pExp) `sepBy` symbol_ ";"),
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

    pArrayVarAssignCallRecord =
      withSrcPos $ do
        x <- lId
        choice
          [ pCall x,
            pRecord x,
            pArrayVarAssign x
          ]
      where
        pArrayVarAssign :: String -> Parser (SourcePos -> UntypedExp)
        pArrayVarAssign x = do
          res <- pArrayAccess x
          case res of
            Left array_e -> pure array_e
            Right f -> do
              let f' = f . SimpleVar x
              choice
                [ do
                    e <- symbol_ ":=" *> pExp
                    pure $ \pos -> AssignExp (f' pos) e pos,
                  pure $ VarExp . f'
                ]

        pCall :: String -> Parser (SourcePos -> UntypedExp)
        pCall x =
          CallExp x
            <$> between
              (symbol_ "(")
              (symbol_ ")")
              (pExp `sepBy` symbol_ ",")

        pRecord :: String -> Parser (SourcePos -> UntypedExp)
        pRecord x =
          let pFields =
                choice
                  [ between (symbol_ "{") (symbol_ "}") $
                      (withSrcPos $ (,,) <$> lId <*> (symbol_ "=" *> pExp)) `sepBy` symbol_ ",",
                    lKeyword "nil" *> pure mempty
                  ]
           in RecordExp x <$> pFields

        pArrayAccess :: String -> Parser (Either (SourcePos -> UntypedExp) (UntypedVar -> UntypedVar))
        pArrayAccess x =
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

        pAccess :: Parser (UntypedVar -> UntypedVar)
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
          <$> (lKeyword "for" *> lId)
          <*> (symbol_ ":=" *> pExp)
          <*> (lKeyword "to" *> pExp)
          <*> (lKeyword "do" *> pExp)

    pLet =
      withSrcPos $
        LetExp
          <$> (lKeyword "let" *> manyTill pDec (lKeyword "in"))
          <*> ( ( SeqExp
                    <$> (withSrcPos ((,) <$> pExp) `sepBy` symbol_ ";")
                )
                  <* lKeyword "end"
              )
