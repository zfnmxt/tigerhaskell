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

stringExprP :: TigerP Expr
stringExprP = SExpr . concat <$> (    many $ escape
                                <|> listify alpha
                                <|> listify (char ' ')
                               )

-- Note: doesn't support control characters
escapeChar :: TigerP Char
escapeChar = choice [ string "\\n" >> return '\n'
                    , string "\\t" >> return '\t'
                    , string "\""  >> return '"'
                    , string "\\"  >> return '\\'
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
integerExprP = IExpr <$> number

noVal :: TigerP Expr
noVal = between (char '(') (char ')') $ return NoValue

keywordP :: TigerP String
keywordP = choice $ map string keywords

kKeywordP :: String -> TigerP String
kKeywordP k = do
  k' <- token keywordP
  if k == k' then return k else reject

typeIdP :: TigerP TypeId
typeIdP = identifierP

typeP :: TigerP Type
typeP = do
  kKeywordP "type"
  typeConst' <- typeIdP
  token $ char '='
  typeBody' <- typeBodyP
  return $ Type typeConst' typeBody'

typeBodyP :: TigerP TypeBody
typeBodyP = choice [ typeArrayP
                   , TypeRecord <$> between (token (char '{')) (token (char '}')) typeFieldsP
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

typeArrayP :: TigerP TypeBody
typeArrayP = do
  kKeywordP "array"
  kKeywordP "of"
  TypeArray <$> typeIdP

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

lValueP :: TigerP LValue
lValueP =
  choice [ lValueFieldP
         , lValueArrayP
         , LId <$> identifierP
         ]
  where lValueFieldP = do
          lval <- lValueP
          token $ char '.'
          id <- identifierP
          return $ LField lval id
        lValueArrayP = do
          lval <- lValueP
          token $ char '['
          expr <- exprP
          token $ char ']'
          return $ LArray lval expr

exprP :: TigerP Expr
exprP =
  choice [ lValueExprP
         , seqExprP
         , stringExprP
         , integerExprP
         , nExprP
         , fExprP
         , ifeExprP
         , ifExprP
         , whileExprP
         , forExprP
         , breakExprP
         , letExprP
         ]

lValueExprP :: TigerP Expr
lValueExprP = LExpr <$> lValueP

seqExprP :: TigerP Expr
seqExprP = do
  token (char '(')
  first <- exprP
  rest  <- many1 $ char ';' >> exprP
  return $ ExprSeq $ first:rest

nExprP :: TigerP Expr
nExprP = NExpr <$> (token (char '-') >> exprP)

fExprP :: TigerP Expr
fExprP = do
  funId <- identifierP
  char '('
  args <- sepBy exprP (char ',')
  char ')'
  return $ FExpr funId args

ifeExprP :: TigerP Expr
ifeExprP = do
  kKeywordP "if"
  cond <- exprP
  kKeywordP "then"
  thenExpr <- exprP
  kKeywordP "else"
  elseExpr <- exprP
  return $ IfE cond thenExpr elseExpr

ifExprP :: TigerP Expr
ifExprP = do
  kKeywordP "if"
  cond <- exprP
  kKeywordP "then"
  thenExpr <- exprP
  return $ If cond thenExpr

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
         return $ Assign id expr

breakExprP :: TigerP Expr
breakExprP = kKeywordP "break" >> return Break

letExprP :: TigerP Expr
letExprP = do
  kKeywordP "let"
  decs <- many decP
  kKeywordP "in"
  exprs <- seqExprP
  kKeywordP "end"
  return $ Let decs exprs

decP :: TigerP Dec
decP =
  choice [ TypeDec <$> typeP
         ,  VarDec <$> varP
         ,  FunDec <$> funP
         ]
