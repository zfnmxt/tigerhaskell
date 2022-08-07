module Lexer.Lexer where

import Control.Monad.Except
import Control.Monad.RWS
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
              Just tk -> [mkToken (S.findMin tk) (envNext env) (envNextLoc env)]
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
                  Just tk -> [mkToken (S.findMin tk) (envNext env) (envNextLoc env)]
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

label :: Ord s => String -> FA m a s p -> FA m a s String
label label fa = fa {payloads = M.fromList $ map (\s -> (s, label)) $ S.toList $ accept fa}

noLabel :: FA m a s p -> FA m a s String
noLabel fa = fa {payloads = M.empty}

lexerDFA :: DFA Char Int (S.Set String)
lexerDFA =
  toDFAFlat $
    FA.unions
      [ label "ID" $ R.toNFA_ ident,
        noLabel $ R.toNFA_ whitespace
      ]

whitespace :: Regex Char
whitespace = Star $ R.oneOf [' ', '\t', '\n']

digit :: Regex Char
digit = R.oneOf ['0' .. '9']

letter :: Regex Char
letter = R.oneOf $ ['a' .. 'z'] ++ ['A' .. 'Z']

ident :: Regex Char
ident = letter ::: Star (R.unions [letter, digit, Sym '_'])
