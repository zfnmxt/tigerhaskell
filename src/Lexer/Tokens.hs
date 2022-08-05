module Lexer.Tokens where

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
