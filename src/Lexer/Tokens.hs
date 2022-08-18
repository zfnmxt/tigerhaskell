module Lexer.Tokens where

import qualified Data.Map as M
import Prelude hiding (EQ, GT, LT)

data Loc = Loc
  { locLine :: Int,
    locCol :: Int
  }
  deriving (Eq, Ord)

instance Show Loc where
  show (Loc l c) = show l ++ ":" ++ show c

instance Semigroup Loc where
  (Loc l1 c1) <> (Loc l2 c2) = Loc (l1 + l2) (c1 + c2)

instance Monoid Loc where
  mempty = Loc 1 1

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
