module Lexer.Lexer where

import Control.Monad.Except
import Control.Monad.RWS
import Data.List (nub)
import Data.Maybe
import qualified Data.Set as S
import Debug.Trace
import Lexer.FA
import qualified Lexer.FA as FA
import qualified Lexer.Regex as R
import Lexer.Tokens
import Lexer.Types
import Prelude hiding (lex)

data PayloadNode s p = PayloadNode
  { payload :: Maybe p,
    payloadNode :: s
  }

instance Eq s => Eq (PayloadNode s p) where
  tn1 == tn2 = payloadNode tn1 == payloadNode tn2

instance Ord s => Ord (PayloadNode s p) where
  tn1 <= tn2 = payloadNode tn1 <= payloadNode tn2

instance Node s => Node (PayloadNode s p) where
  startNode = PayloadNode Nothing startNode
  nextNode tn = tn {payloadNode = nextNode $ payloadNode tn}
  renameFun fas i tn =
    tn {payloadNode = renameFun (map (FA.mapNodes payloadNode) fas) i $ payloadNode tn}

type TokenNode = PayloadNode Int (String -> Loc -> Token)

injectPayload :: (Ord a, Ord s, Functor m) => (s -> Maybe p) -> FA m a s -> FA m a (PayloadNode s p)
injectPayload f = mapNodes (\s -> PayloadNode (f s) s)

injectToken :: Functor m => (String -> BaseToken) -> FA m Char Int -> FA m Char TokenNode
injectToken mkToken =
  injectPayload $ const $ Just $ \s l -> Token (mkToken s) l

data Env = Env
  { envNext :: String,
    envRest :: String,
    envLoc :: Loc,
    envNode :: S.Set TokenNode
  }

type LexM = RWST () [Token] Env (Either String)

runLexM :: LexM a -> String -> Either String (a, [Token])
runLexM m s =
  evalRWST m () $
    Env
      { envNext = "",
        envRest = s,
        envLoc = initLoc,
        envNode = start lexerDFA
      }

lex :: LexM ()
lex = do
  env <- get
  case envRest env of
    "" -> pure ()
    (c : cs) -> do
      traceM $ envNext env
      case step lexerDFA c (envNode env) of
        Nothing -> do
          unless (envNode env `S.member` accept lexerDFA) $
            throwError "No lex."

          put
            Env
              { envNext = [c],
                envRest = cs,
                envLoc = updateLoc (envLoc env) c,
                envNode = fromMaybe (error "") $ step lexerDFA c (start lexerDFA)
              }

          let f = fromMaybe (error "") $ payload $ S.elemAt 0 $ envNode env
              token = [f (envNext env) (envLoc env)]

          tell token
          lex
        Just s' -> do
          put
            Env
              { envNext = envNext env ++ [c],
                envRest = cs,
                envLoc = updateLoc (envLoc env) c,
                envNode = s'
              }
          lex
  where
    updateLoc :: Loc -> Char -> Loc
    updateLoc loc '\n' =
      Loc
        { locLine = locLine loc + 1,
          locCol = 0
        }
    updateLoc loc _ = loc {locCol = locCol loc + 1}

lexerDFA :: DFA Char (S.Set TokenNode)
lexerDFA =
  toDFA $
    FA.unions
      [ injectToken ID $ R.toNFA_ ident
      ]

whitespace :: Regex Char
whitespace = Star $ R.oneOf [' ', '\t', '\n']

digit :: Regex Char
digit = R.oneOf ['0' .. '9']

letter :: Regex Char
letter = R.oneOf $ ['a' .. 'z'] ++ ['A' .. 'Z']

ident :: Regex Char
ident = letter ::: (R.unions [letter, digit, Sym '_'])
