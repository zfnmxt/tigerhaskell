module Lexer.Tokens where

import qualified Data.Map as M
import Prelude hiding (EQ, GT, LT)

data Loc = Loc
  { locLine :: Int,
    locCol :: Int
  }
  deriving (Show, Eq, Ord)

initLoc = Loc 0 0

class Located a where
  loc :: a -> Loc

instance Located Token where
  loc = tokenLoc

data Token = Token {token :: BaseToken, tokenLoc :: Loc} deriving (Show, Eq, Ord)

data BaseToken
  = TYPE
  | VAR
  | FUNCTION
  | BREAK
  | OF
  | END
  | IN
  | NIL
  | LET
  | DO
  | TO
  | FOR
  | WHILE
  | ELSE
  | THEN
  | IF
  | ARRAY
  | ASSIGN
  | OR
  | AND
  | GE
  | GT
  | LE
  | LT
  | NEQ
  | EQ
  | DIVIDE
  | TIMES
  | MINUS
  | PLUS
  | DOT
  | RBRACE
  | LBRACE
  | RBRACK
  | LBRACK
  | RPAREN
  | LPAREN
  | SEMICOLON
  | COLON
  | COMMA
  | STRING String
  | INT Int
  | ID String
  | EOF
  deriving (Show, Eq, Ord)

mkToken :: String -> String -> Loc -> Token
mkToken t s = Token (toBaseToken t s)
  where
    toBaseToken :: String -> String -> BaseToken
    toBaseToken t s =
      case t of
        "TYPE" -> TYPE
        "VAR" -> VAR
        "FUNCTION" -> FUNCTION
        "BREAK" -> BREAK
        "OF" -> OF
        "END" -> END
        "IN" -> IN
        "NIL" -> NIL
        "LET" -> LET
        "DO" -> DO
        "TO" -> TO
        "FOR" -> FOR
        "WHILE" -> WHILE
        "ELSE" -> ELSE
        "THEN" -> THEN
        "IF" -> IF
        "ARRAY" -> ARRAY
        "ASSIGN" -> ASSIGN
        "OR" -> OR
        "AND" -> AND
        "GE" -> GE
        "GT" -> GT
        "LE" -> LE
        "LT" -> LT
        "NEQ" -> NEQ
        "EQ" -> EQ
        "DIVIDE" -> DIVIDE
        "TIMES" -> TIMES
        "MINUS" -> MINUS
        "PLUS" -> PLUS
        "DOT" -> DOT
        "RBRACE" -> RBRACE
        "LBRACE" -> LBRACE
        "RBRACK" -> RBRACK
        "LBRACK" -> LBRACK
        "RPAREN" -> RPAREN
        "LPAREN" -> LPAREN
        "SEMICOLON" -> SEMICOLON
        "COLON" -> COLON
        "COMMA" -> COMMA
        "STRING" -> STRING s
        "INT" -> INT $ read s
        "ID" -> ID s
        "EOF" -> EOF
