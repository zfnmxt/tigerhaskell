{-# LANGUAGE FlexibleInstances #-}

module Parser where

import DumbParser
import TigerDef
import AST
import Data.Char (chr)

type TigerError = String
type TigerP = Parser TigerError

instance Error TigerError where
  errEmpty = ""
  errFail env = "Parser failure at " ++ line ++ ":" ++ col
    where line = show $ linenum env
          col  = show $ colnum env

identifierP :: TigerP String
identifierP = do
  x <- token letter
  rest <- many $ choice [letter, digit, char '_']
  if (x:rest) `elem` keywords
    then reject
    else return $ x:rest

comment :: TigerP ()
comment = between (string "/*") (string "*/") body
  where body = void $ manyTo (comment <|> throw)  "*/"

-- Note: doesn't support control characters

keywordP :: TigerP String
keywordP = choice $ map string keywords

kKeywordP :: String -> TigerP String
kKeywordP k = do
  k' <- token keywordP
  if k == k' then return k else reject

typeIdP :: TigerP TypeId
typeIdP = identifierP

seqExprP :: TigerP Expr
seqExprP = do
  first <- exprP
  rest  <- many1 $ char ';' >> exprP
  return $ ExprSeq $ first:rest

exprP :: TigerP Expr
exprP = token $ between (token (char '(')) (token (char ')')) seqExprP
                <|> orP

orP :: TigerP Expr
orP = token $ chainl1 andP (token (char '|') >> return (BExpr Or))

andP :: TigerP Expr
andP = chainl1 comparisonP (token (char '&') >> return (BExpr And))

comparisonP :: TigerP Expr
comparisonP = comparisonP' <|> addSubP
  where comparisonP' = do
           l  <- addSubP
           op <- choice $ map (\s -> token (string s)) [">=", "<=", "=", "<>", ">", "<"]
           r  <- addSubP
           case op of
             "="  -> return $ BExpr Equal  l r
             "<>" -> return $ BExpr NEqual l r
             ">"  -> return $ BExpr Gt     l r
             "<"  -> return $ BExpr Lt     l r
             ">=" -> return $ BExpr GTE    l r
             "<=" -> return $ BExpr LTE    l r

addSubP :: TigerP Expr
addSubP = chainl1 multDivP $ do
             op <- token (satisfy (`elem` ['+', '-']))
             case op of
               '+' -> return $ BExpr Add
               '-' -> return $ BExpr Sub

multDivP :: TigerP Expr
multDivP = chainl1 negExprP $ do
             op <- token (satisfy (`elem` ['*', '/']))
             case op of
               '*' -> return $ BExpr Mult
               '/' -> return $ BExpr Div

negExprP :: TigerP Expr
negExprP = (token (char '-') >> NExpr <$> primaryP )
           <|> parenP

parenP :: TigerP Expr
parenP = between (token (char '(')) (token (char ')')) (exprP <|> return NoValue)
         <|> primaryP

primaryP :: TigerP Expr
primaryP = choice
           [ ifExprP
           , whileExprP
           , forExprP
           , letExprP
           , breakExprP
           , baseExprP
           ]

ifExprP :: TigerP Expr
ifExprP = do
  kKeywordP "if"
  cond <- exprP
  kKeywordP "then"
  thenExpr <- exprP
  let elseP = option (Just <$> (kKeywordP "else" >> exprP)) Nothing
  elseExpr <- elseP
  case elseExpr of
    Nothing       -> return $ If cond thenExpr
    Just elseExpr -> return $ IfE cond thenExpr elseExpr

whileExprP :: TigerP Expr
whileExprP = do
  kKeywordP "while"
  cond <- exprP
  kKeywordP "do"
  expr <- exprP
  return $ While cond expr

forExprP :: TigerP Expr
forExprP = do
  kKeywordP "for"
  assign <- forAssignP
  kKeywordP "to"
  limit <- exprP
  kKeywordP "do"
  expr <- exprP
  return $ For assign limit expr
  where forAssignP = do
         id <- identifierP
         token $ string ":="
         expr <- exprP
         return $ Assign (LId id) expr

letExprP :: TigerP Expr
letExprP = do
  kKeywordP "let"
  decs <- many decP
  kKeywordP "in"
  exprs <- sepBy exprP (token (char ';'))
  kKeywordP "end"
  return $ Let decs exprs

decP :: TigerP Dec
decP = token $ choice [ TypeDec <$> typeP
                      ,  VarDec <$> varP
                      ,  FunDec <$> funP
                      ]

typeP :: TigerP Type
typeP = do
  kKeywordP "type"
  typeConst' <- typeIdP
  token $ char '='
  typeBody' <- typeBodyP
  return $ Type typeConst' typeBody'

typeBodyP :: TigerP TypeBody
typeBodyP = choice [ arrayTypeP
                   , RecordType <$> between (token (char '{')) (token (char '}')) typeFieldsP
                   , DataConst  <$> typeIdP
                   ]

typeFieldsP :: TigerP [TypeField]
typeFieldsP =
  let idTypeP = do
        id <- identifierP
        token $ char ':'
        type' <- typeIdP
        return $ TypeField id type'
  in do first <- idTypeP
        rest  <- many $ (token (char ',')) >> idTypeP
        return $ first:rest
  <|> return []

arrayTypeP :: TigerP TypeBody
arrayTypeP = do
  kKeywordP "array"
  kKeywordP "of"
  ArrayType <$> typeIdP

typeAnnoP :: TigerP (Maybe TypeId)
typeAnnoP = maybeP $ (token (char ':') >> typeIdP)

varP :: TigerP Var
varP = do
  kKeywordP "var"
  id <- identifierP
  typeAnno <- typeAnnoP
  token $ string ":="
  expr <- exprP
  return $ Var id typeAnno expr

funP :: TigerP Fun
funP = do
  kKeywordP "function"
  id <- identifierP
  token $ char '('
  args <- typeFieldsP
  token $ char ')'
  typeAnno <- typeAnnoP
  token $ char '='
  expr <- exprP
  return $ Fun id args typeAnno expr

breakExprP :: TigerP Expr
breakExprP = kKeywordP "break" >> return Break

baseExprP :: TigerP Expr
baseExprP = choice
            [ arrayExprP
            , recordExprP
            , fCallExprP
            , lValueExprP
            , stringExprP
            , integerExprP
            ]

arrayExprP :: TigerP Expr
arrayExprP = do
  type' <- typeIdP
  token $ char '['
  n <- exprP
  kKeywordP "of"
  v <- exprP
  return $ Array type' n v

recordExprP :: TigerP Expr
recordExprP = do
  type' <- typeIdP
  token $ char '{'
  fields <- sepBy1 fieldP (char ',')
  return $ Record type' fields
  where fieldP = do
          id <- identifierP
          char '='
          expr <- exprP
          return $ RecordField id expr

lFieldP :: TigerP (Either Id Expr)
lFieldP = do
  token $ char '.'
  id <- identifierP
  return $ Left id

lArrayP :: TigerP (Either Id Expr)
lArrayP = do
  token $ char '['
  expr <- exprP
  token $ char ']'
  return $ Right expr

lValueExprP :: TigerP Expr
lValueExprP = LExpr <$> lValueP

lValueP :: TigerP LValue
lValueP = do
  id <- identifierP
  rest <- many $ lFieldP <|> lArrayP
  let lid = LId id
  case rest of
    []     -> return $ lid
    rest'  -> return $ foldl f lid rest'
      where f lval next =
              case next of
                Left id    -> LField lval id
                Right expr -> LArray lval expr

stringExprP :: TigerP Expr
stringExprP = do
  token $ char '\"'
  strs <- many $ listify alpha <|> listify (char ' ') <|> escape
  token $ char '\"'
  return $ SExpr (concat strs)

escapeChar :: TigerP Char
escapeChar = choice [ string "\\n" >> return '\n'
                    , string "\\t" >> return '\t'
                    , string "\\\""  >> return '"'
                    , string "\\\\"  >> return '\\'
                    ]

escape :: TigerP String
escape = do
  choice $ [ multiline
           , listify escapeChar
           , ascii
           ]
    where ascii      = listify $ char '\\' >> chr . read <$> parseN 3 digit
          multiline  = between (char '\\') (char '\\') whitespace >> return ""

integerExprP :: TigerP Expr
integerExprP = token $ IExpr <$> number

fCallExprP :: TigerP Expr
fCallExprP = do
  funId <- identifierP
  char '('
  args <- sepBy exprP (char ',')
  char ')'
  return $ FCall funId args

assignExprP :: TigerP Expr
assignExprP = do
  lval <- lValueP
  token $ string ":="
  expr <- exprP
  return $ Assign lval expr
