module Lexer.Lexer where

import Control.Monad.Except
import Control.Monad.RWS
import Data.Char
import Data.List (nub)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Debug.Trace
import Lexer.FA
import qualified Lexer.FA as FA
import qualified Lexer.Regex as R
import Lexer.Tokens
import Lexer.Types
import Prelude hiding (lex)

data Env = Env
  { envNext :: String,
    envNextLoc :: Loc,
    envRest :: String,
    envLoc :: Loc,
    envNode :: Int
  }

type LexM = RWST () [Token] Env (Either String)

lex :: String -> Either String [Token]
lex = fmap snd . runLexM lexer

runLexM :: LexM a -> String -> Either String (a, [Token])
runLexM m s =
  evalRWST m () $
    Env
      { envNext = "",
        envNextLoc = initLoc,
        envRest = s,
        envLoc = initLoc,
        envNode = start lexerDFA
      }

lexer :: LexM ()
lexer = do
  env <- get
  case envRest env of
    "" -> do
      unless (envNode env `S.member` accept lexerDFA) $
        throwError "No lex."

      let token =
            case payloads lexerDFA M.!? envNode env of
              Just tk -> [mkToken tk (envNext env) (envNextLoc env)]
              Nothing -> []

      tell token
    (c : cs) -> do
      case step lexerDFA c (envNode env) of
        Nothing -> do
          unless (envNode env `S.member` accept lexerDFA) $
            throwError "No lex."

          put
            Env
              { envNext = [c],
                envNextLoc = updateLoc (envLoc env) c,
                envRest = cs,
                envLoc = updateLoc (envLoc env) c,
                envNode = fromMaybe (error "") $ step lexerDFA c (start lexerDFA)
              }

          let token =
                case payloads lexerDFA M.!? envNode env of
                  Just tk -> [mkToken tk (envNext env) (envNextLoc env)]
                  Nothing -> []

          tell token
          lexer
        Just s' -> do
          put
            env
              { envNext = envNext env ++ [c],
                envRest = cs,
                envLoc = updateLoc (envLoc env) c,
                envNode = s'
              }
          lexer
  where
    updateLoc :: Loc -> Char -> Loc
    updateLoc loc '\n' =
      Loc
        { locLine = locLine loc + 1,
          locCol = 0
        }
    updateLoc loc _ = loc {locCol = locCol loc + 1}

class Label a where
  label :: String -> a -> NFA Char Int String
  ignore :: a -> NFA Char Int String

instance Label (Regex Char) where
  label name r = nfa {payloads = M.fromList $ map (\s -> (s, name)) $ S.toList $ accept nfa}
    where
      nfa = R.toNFA r
  ignore r = (R.toNFA r) {payloads = M.empty}

instance Label (NFA Char Int String) where
  label name nfa = nfa {payloads = M.fromList $ map (\s -> (s, name)) $ S.toList $ accept nfa}
  ignore nfa = nfa {payloads = M.empty}

lexerDFA :: DFA Char Int String
lexerDFA =
  toDFA $
    FA.unions
      [ label "TYPE" $ R.lit "type",
        label "VAR" $ R.lit "var",
        label "FUNCTION" $ R.lit "function",
        label "BREAK" $ R.lit "break",
        label "OF" $ R.lit "of",
        label "END" $ R.lit "end",
        label "IN" $ R.lit "in",
        label "NIL" $ R.lit "nil",
        label "LET" $ R.lit "let",
        label "DO" $ R.lit "do",
        label "TO" $ R.lit "to",
        label "FOR" $ R.lit "for",
        label "WHILE" $ R.lit "while",
        label "ELSE" $ R.lit "else",
        label "THEN" $ R.lit "then",
        label "IF" $ R.lit "if",
        label "ARRAY" $ R.lit "array",
        label "ASSIGN" $ R.lit ":=",
        label "OR" $ R.lit "|",
        label "AND" $ R.lit "&",
        label "GE" $ R.lit ">=",
        label "GT" $ R.lit ">",
        label "LE" $ R.lit "<=",
        label "LT" $ R.lit "<",
        label "NEQ" $ R.lit "<>",
        label "EQ" $ R.lit "=",
        label "DIVIDE" $ R.lit "/",
        label "TIMES" $ R.lit "*",
        label "MINUS" $ R.lit "-",
        label "PLUS" $ R.lit "+",
        label "DOT" $ R.lit ".",
        label "RBRACE" $ R.lit "}",
        label "LBRACE" $ R.lit "{",
        label "RBRACK" $ R.lit "]",
        label "LBRACK" $ R.lit "[",
        label "RPAREN" $ R.lit ")",
        label "LPAREN" $ R.lit "(",
        label "SEMICOLON" $ R.lit ";",
        label "COLON" $ R.lit ":",
        label "COMMA" $ R.lit ",",
        label "ID" $ FA.oneOf letters `FA.concat` FA.star (FA.oneOf $ letters ++ digits ++ "_"),
        label "INT" $ FA.plus $ FA.oneOf digits,
        ignore whitespace
      ]

whitespace :: Regex Char
whitespace = Star $ R.oneOf [' ', '\t', '\n']

digits :: [Char]
digits = ['0' .. '9']

letters :: [Char]
letters = filter isLetter [minBound .. maxBound]
