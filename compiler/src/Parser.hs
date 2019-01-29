{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Parser where

import DumbParser
import TigerDef
import AST
import Data.Char (chr, isPrint)

type TigerError = String
type TigerP = Parser TigerError

initialEnv :: Env
initialEnv = Env 0 0

-- lalalalla
tigerParse :: TigerP a -> String -> Either TigerError a
tigerParse p s =
  case runParser p initialEnv s of
    Left x -> Left x
    Right (x, _) -> Right x

instance Error TigerError where
  errEmpty = ""
  errFail env = "Parser failure at " ++ line ++ ":" ++ col
    where line = show $ linenum env
          col  = show $ colnum env

ctoken' :: TigerP a -> TigerP a
ctoken' = ctoken commentP

identifierP :: TigerP String
identifierP = do
  x <- ctoken' letter
  rest <- many $ choice [letter, digit, char '_']
  if (x:rest) `elem` keywords
    then reject
    else return $ x:rest


commentP :: TigerP ()
commentP = between (string "/*") (string "*/") body
  where body = void $ manyTo (commentP <|> throw)  "*/"

keywordP :: TigerP String
keywordP = choice $ map string keywords

kKeywordP :: String -> TigerP String
kKeywordP k = do
  k' <- ctoken' keywordP
  if k == k' then return k else reject

typeIdP :: TigerP TypeId
typeIdP = identifierP

seqExprP :: TigerP Expr
seqExprP = do
  first <- exprP
  rest  <- many1 $ char ';' >> exprP
  return $ ExprSeq $ first:rest

exprP :: TigerP Expr
exprP = do
        expr <- between (ctoken' (char '(')) (ctoken' (char ')')) seqExprP
                <|> orP
        ctoken' (return expr)

orP :: TigerP Expr
orP = ctoken' $ chainl1 andP (ctoken' (char '|') >> return (\e1 e2 -> IfE e1 (IExpr 1) e2))

andP :: TigerP Expr
andP = chainl1 comparisonP (ctoken' (char '&') >> return (\e1 e2 -> IfE e1 e2 (IExpr 0)))

comparisonP :: TigerP Expr
comparisonP = comparisonP' <|> addSubP
  where comparisonP' = do
           l  <- addSubP
           op <- choice $ map (\s -> ctoken' (string s)) [">=", "<=", "=", "<>", ">", "<"]
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
             op <- ctoken' (satisfy (`elem` ['+', '-']))
             case op of
               '+' -> return $ BExpr Add
               '-' -> return $ BExpr Sub

multDivP :: TigerP Expr
multDivP = chainl1 negExprP $ do
             op <- ctoken' (satisfy (`elem` ['*', '/']))
             case op of
               '*' -> return $ BExpr Mult
               '/' -> return $ BExpr Div

negExprP :: TigerP Expr
negExprP = (ctoken' (char '-') >> NExpr <$> primaryP )
           <|> parenP

parenP :: TigerP Expr
parenP = between (ctoken' (char '(')) (ctoken' (char ')')) (exprP <|> return UnitExpr)
         <|> primaryP

primaryP :: TigerP Expr
primaryP = choice
           [ ifExprP
           , whileExprP
           , forExprP
           , letExprP
           , breakExprP
           , assignExprP
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
         ctoken' $ string ":="
         expr <- exprP
         return $ Assign (SimpleVar id) expr

letExprP :: TigerP Expr
letExprP = do
  kKeywordP "let"
  decs <- many decP
  kKeywordP "in"
  exprs <- sepBy exprP (ctoken' (char ';'))
  kKeywordP "end"
  return $ Let decs exprs

decP :: TigerP Dec
decP = ctoken' $ choice [ TypeDec <$> many1 typeP
                      ,  VarDec <$> varP
                      ,  FunDec <$> many1 funP
                      ]

typeP :: TigerP Type
typeP = do
  kKeywordP "type"
  typeConst' <- typeIdP
  ctoken' $ char '='
  typeBody' <- typeBodyP
  return $ Type typeConst' typeBody'

typeBodyP :: TigerP TypeBody
typeBodyP = choice [ arrayTypeP
                   , RecordType <$> between (ctoken' (char '{')) (ctoken' (char '}')) typeFieldsP
                   , DataConst  <$> typeIdP
                   ]

typeFieldsP :: TigerP [TypeField]
typeFieldsP =
  let idTypeP = do
        id <- identifierP
        ctoken' $ char ':'
        type' <- typeIdP
        return $ id |: type'
  in do first <- idTypeP
        rest  <- many $ (ctoken' (char ',')) >> idTypeP
        return $ first:rest
  <|> return []

fieldsP :: TigerP [Field]
fieldsP = do
  typeFields <- typeFieldsP
  return $ f <$> typeFields
  where f TypeField{..} = Field _typeFieldId _typeFieldType True



arrayTypeP :: TigerP TypeBody
arrayTypeP = do
  kKeywordP "array"
  kKeywordP "of"
  ArrayType <$> typeIdP

typeAnnoP :: TigerP (Maybe TypeId)
typeAnnoP = maybeP $ (ctoken' (char ':') >> typeIdP)

varP :: TigerP VarDef
varP = do
  kKeywordP "var"
  id <- identifierP
  typeAnno <- typeAnnoP
  ctoken' $ string ":="
  expr <- exprP
  return $ VarDef id typeAnno expr

funP :: TigerP FunDef
funP = do
  kKeywordP "function"
  id <- identifierP
  ctoken' $ char '('
  args <- fieldsP
  ctoken' $ char ')'
  typeAnno <- typeAnnoP
  ctoken' $ char '='
  expr <- exprP
  return $ FunDef id args typeAnno expr

breakExprP :: TigerP Expr
breakExprP = kKeywordP "break" >> return Break

baseExprP :: TigerP Expr
baseExprP = choice
            [ arrayExprP
            , recordExprP
            , fCallExprP
            , nilExprP
            , lValueExprP
            , stringExprP
            , integerExprP
            ]

nilExprP :: TigerP Expr
nilExprP = kKeywordP "nil" >> return NilExpr

arrayExprP :: TigerP Expr
arrayExprP = do
  type' <- typeIdP
  ctoken' $ char '['
  n <- exprP
  ctoken' $ char ']'
  kKeywordP "of"
  v <- exprP
  return $ ArrayExpr type' n v

recordExprP :: TigerP Expr
recordExprP = do
  type' <- typeIdP
  ctoken' $ char '{'
  fields <- sepBy1 fieldP (char ',')
  ctoken' $ char '}'
  return $ RecordExpr type' fields
  where fieldP = do
          id <- identifierP
          char '='
          expr <- exprP
          return $ id |. expr

lFieldP :: TigerP (Either Id Expr)
lFieldP = do
  ctoken' $ char '.'
  id <- identifierP
  return $ Left id

lArrayP :: TigerP (Either Id Expr)
lArrayP = do
  ctoken' $ char '['
  expr <- exprP
  ctoken' $ char ']'
  return $ Right expr

lValueExprP :: TigerP Expr
lValueExprP = VExpr <$> lValueP

lValueP :: TigerP Var
lValueP = do
  id <- identifierP
  rest <- many $ lFieldP <|> lArrayP
  let lid = SimpleVar id
  case rest of
    []     -> return $ lid
    rest'  -> return $ foldl f lid rest'
      where f lval next =
              case next of
                Left id    -> FieldVar lval id
                Right expr -> ArrayVar lval expr

strExprP' :: TigerP Char
strExprP' = satisfy (isPrint)

stringExprP :: TigerP Expr
stringExprP = do
  ctoken' $ char '\"'
  strs <- many $ listify printable <|> listify (char ' ') <|> escape
  ctoken' $ char '\"'
  return $ SExpr (concat strs)
    where printable = satisfy (\c -> isPrint c && (c `notElem` ['\"', '\\']))

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
integerExprP = ctoken' $ IExpr <$> number
  
fCallExprP :: TigerP Expr
fCallExprP = do
  funId <- identifierP
  char '('
  args <- sepBy exprP (ctoken' (char ','))
  ctoken' $ char ')'
  return $ FCall funId args

assignExprP :: TigerP Expr
assignExprP = do
  lval <- lValueP
  ctoken' $ string ":="
  expr <- exprP
  return $ Assign lval expr
