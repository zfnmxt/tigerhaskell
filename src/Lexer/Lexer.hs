{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lexer.Lexer where

import Control.Monad.Except
import Control.Monad.RWS
import Control.Monad.Trans.Maybe
import Data.Char
import Data.List (nub)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Debug.Trace
import Lexer.FA (Env (..), MonadDFA (..))
import qualified Lexer.FA as FA
import qualified Lexer.Finite as F
import qualified Lexer.Regex as R
import Lexer.Tokens
import Lexer.Types hiding (DFA, NFA)
import qualified Lexer.Types as T
import Prelude hiding (EQ, GT, LT, lex)

type Label = String

type Priority = Int

type EdgeLabel = (Priority, String)

type DFA = T.DFA Char Int EdgeLabel

type NFA = T.NFA Char Int EdgeLabel

type NodeEnv = Env Char Int Loc

type NodeM = RWST DFA () NodeEnv (Either String)

data Node = Node
  { nodeLabel :: Label,
    nodeDFA :: DFA,
    nodeActions :: ActionMap
  }

data LexEnvMisc = LexEnvMisc
  { lexLoc :: Loc,
    lexCommentDepth :: Int,
    lexInString :: Bool
  }

type LexEnv = Env Char Label LexEnvMisc

type LexM = RWST Lexer [Token] LexEnv (Either String)

newtype Action = Action {runAction :: String -> Loc -> LexM [Token]}

type ActionMap = M.Map EdgeLabel Action

type Lexer = T.DFA EdgeLabel Label (DFA, ActionMap)

modifyMisc :: (Monoid w, Monad m) => (g -> g) -> RWST r w (Env a s g) m ()
modifyMisc f = modify $ \env -> env {envMisc = f $ envMisc env}

getsMisc :: (Monoid w, Monad m) => (g -> h) -> RWST r w (Env a s g) m h
getsMisc f = gets (f . envMisc)

lexError :: String -> LexM a
lexError msg = do
  loc <- getsMisc lexLoc
  throwError $ "Lexing error:" <> show loc <> ": " <> msg

nodeError :: String -> NodeM a
nodeError msg = do
  loc <- gets envMisc
  throwError $ "Lexing error:" <> show loc <> ": " <> msg

lex :: String -> Either String [Token]
lex = fmap snd . evalLexM runLexer

evalLexM :: LexM a -> String -> Either String (a, [Token])
evalLexM m s =
  evalRWST m lexer $
    Env
      { envConsumed = mempty,
        envRest = s,
        envState = start lexer,
        envMisc =
          LexEnvMisc
            { lexLoc = mempty,
              lexCommentDepth = 0,
              lexInString = False
            }
      }

runLexer :: LexM ()
runLexer = do
  Env _ rest label (LexEnvMisc loc commentDepth inString) <- get
  case rest of
    "" -> do
      env <- get
      unless (commentDepth == 0) $
        lexError "Unclosed comment."

      when inString $
        lexError "Unclosed string."

      tell [Token EOF loc]
    as -> do
      let (dfa, actions) = lexer `FA.lookupPayload` label
          nEnv =
            Env
              { envConsumed = mempty,
                envRest = rest,
                envState = start dfa,
                envMisc = loc
              }
      (edgeLabel, nEnv') <- lift $ runNodeM runNode dfa nEnv
      action <- lookupAction edgeLabel
      token <- runAction action (envConsumed nEnv') loc
      tell token
      modify
        ( \env' ->
            envUpdLoc (const $ envLoc nEnv') $
              env'
                { envConsumed = envConsumed env' ++ envConsumed nEnv',
                  envRest = envRest nEnv'
                }
        )
      runLexer

lexer :: Lexer
lexer =
  FA
    { delta = mempty,
      delta_e = mempty,
      start = head labels,
      accept = S.fromList labels,
      states = S.fromList labels,
      alphabet = S.fromList $ M.keys $ mconcat $ map snd dfasActions,
      payloads = M.fromList $ zip labels dfasActions
    }
  where
    labels = map nodeLabel nodes
    dfasActions = map (\n -> (nodeDFA n, nodeActions n)) nodes
    nodes =
      [ startNode,
        commentNode,
        stringNode
      ]

runNodeM :: NodeM a -> DFA -> NodeEnv -> Either String (a, NodeEnv)
runNodeM m dfa nenv = do
  (a, s, _) <- runRWST m dfa nenv
  pure (a, s)

runNode :: NodeM EdgeLabel
runNode = FA.run onStep accept reject
  where
    onStep c = modify $ envUpdLoc $ updateLoc c
    accept = lookupEdge
    reject = nodeError "No lex."

mkEdge ::
  Label ->
  Priority ->
  (Label, T.NFA Char Int Label, Action) ->
  (NFA, ActionMap)
mkEdge label priority (edgeLabel, nfa, action) =
  (nfa {payloads = payloads'}, actions)
  where
    payloads' =
      M.fromList $
        map (\s -> (s, (priority, edgeLabel))) $
          S.toList $
            accept nfa
    actions = M.singleton (priority, edgeLabel) action

mkNode :: Label -> [(Label, T.NFA Char Int Label, Action)] -> Node
mkNode label = squash . zipWith (mkEdge label) [0 ..]
  where
    squash edges =
      let (nfas, ms) = unzip edges
       in Node
            { nodeLabel = label,
              nodeDFA = FA.toDFA $ FA.unions nfas,
              nodeActions = mconcat ms
            }

lookupEdge :: NodeM EdgeLabel
lookupEdge = do
  dfa <- ask
  state <- gets envState
  pure $ dfa `FA.lookupPayload` state

lookupAction :: EdgeLabel -> LexM Action
lookupAction edgeLabel = do
  dfa <- ask
  label <- gets envState
  let (_, actions) = dfa `FA.lookupPayload` label
  pure $ actions M.! edgeLabel

class EnvLoc a where
  envLoc :: a -> Loc
  envUpdLoc :: (Loc -> Loc) -> a -> a

instance EnvLoc NodeEnv where
  envLoc = envMisc
  envUpdLoc f env = env {envMisc = f $ envLoc env}

instance EnvLoc LexEnv where
  envLoc = lexLoc . envMisc
  envUpdLoc f env = env {envMisc = (envMisc env) {lexLoc = f $ envLoc env}}

updateLoc :: Char -> Loc -> Loc
updateLoc '\n' loc =
  Loc
    { locLine = locLine loc + 1,
      locCol = 0
    }
updateLoc _ loc = loc {locCol = locCol loc + 1}

whitespace :: [Char]
whitespace = filter isSpace [minBound .. maxBound]

digits :: [Char]
digits = ['0' .. '9']

letters :: [Char]
letters = ['A' .. 'Z'] ++ ['a' .. 'z']

-- letters = filter isLetter [minBound .. maxBound]

printable :: [Char]
printable = letters ++ digits

-- printable = filter isPrint [minBound .. maxBound]

lexerAlphabet :: [Char]
lexerAlphabet = printable ++ whitespace

-- lexerAlphabet = filter (\c -> isPrint c || isSpace c) [minBound .. maxBound]

stringNode :: Node
stringNode =
  mkNode
    "STRING"
    [ ("\"", R.toNFA $ Sym '\"', end_action),
      ( "body",
        FA.plus $
          FA.oneOf (' ' : (printable L.\\ ['\\', '\"']))
            `FA.union` R.toNFA
              ( Sym '\\'
                  ::: R.unions
                    ( [ R.plus (R.oneOf [' ', '\t', '\n', '\f'] ::: Sym '\\'),
                        (Sym '^' ::: R.oneOf ['A' .. 'Z'])
                      ]
                        ++ map R.lit ["n", "t", "ddd", "\"", "\\"]
                    )
              ),
        Action $ \s l -> pure [Token (STRING $ read $ "\"" ++ s ++ "\"") l]
      )
    ]
  where
    end_action = Action $ \s l -> do
      modifyMisc $ \lexMisc -> lexMisc {lexInString = False}
      modify $ \env -> env {envState = "START"}
      pure mempty

commentNode :: Node
commentNode =
  mkNode
    "COMMENT"
    [ ("*/", R.toNFA $ R.lit "*/", end_action),
      ("/*", R.toNFA $ R.lit "/*", start_action),
      (".", FA.plus $ FA.oneOf lexerAlphabet, Action $ (const . const) (pure mempty))
    ]
  where
    end_action = Action $ \s l -> do
      modifyMisc $ \lexMisc -> lexMisc {lexCommentDepth = lexCommentDepth lexMisc - 1}
      commentDepth <- getsMisc lexCommentDepth
      when (commentDepth < 0) $
        lexError "Missing /*"
      let s'
            | commentDepth > 0 = "COMMENT"
            | otherwise = "START"
      modify $ \env -> env {envState = s'}
      pure mempty
    start_action = Action $ \s l -> do
      modifyMisc $ \lexMisc -> lexMisc {lexCommentDepth = lexCommentDepth lexMisc + 1}
      pure mempty

startNode :: Node
startNode =
  mkNode
    "START"
    $ ( map
          (\(bt, s) -> (show bt, R.toNFA $ R.lit s, Action $ \_ l -> pure [Token bt l]))
          [ (TYPE, "type"),
            (VAR, "var"),
            (FUNCTION, "function"),
            (BREAK, "break"),
            (OF, "of"),
            (END, "end"),
            (IN, "in"),
            (NIL, "nil"),
            (LET, "let"),
            (DO, "do"),
            (TO, "to"),
            (FOR, "for"),
            (WHILE, "while"),
            (ELSE, "else"),
            (THEN, "then"),
            (IF, "if"),
            (ARRAY, "array"),
            (ASSIGN, ":="),
            (OR, "|"),
            (AND, "&"),
            (GE, ">="),
            (GT, ">"),
            (LE, "<="),
            (LT, "<"),
            (NEQ, "<>"),
            (EQ, "="),
            (DIVIDE, "/"),
            (TIMES, "*"),
            (MINUS, "-"),
            (PLUS, "+"),
            (DOT, "."),
            (RBRACE, "}"),
            (LBRACE, "{"),
            (RBRACK, "]"),
            (LBRACK, "["),
            (RPAREN, ")"),
            (LPAREN, "("),
            (SEMICOLON, ";"),
            (COLON, ":"),
            (COMMA, ",")
          ]
      )
      ++ [ ("int", FA.plus $ FA.oneOf digits, Action $ \s l -> pure [Token (INT $ read s) l]),
           ( "id",
             FA.oneOf letters `FA.concat` FA.plus (FA.oneOf $ concat [letters, digits, "_"]),
             Action $ \s l -> pure [Token (ID s) l]
           ),
           ("/*", R.toNFA $ R.lit "/*", comment_start_action),
           ("\"", R.toNFA $ Sym '\"', string_start_action),
           ("whitespace", FA.star $ FA.oneOf whitespace, Action $ (const . const) (pure mempty))
         ]
  where
    comment_start_action = Action $ \s l -> do
      modifyMisc $ \lexMisc -> lexMisc {lexCommentDepth = lexCommentDepth lexMisc + 1}
      modify $ \env -> env {envState = "COMMENT"}
      pure mempty
    string_start_action = Action $ \s l -> do
      modifyMisc $ \lexMisc -> lexMisc {lexInString = True}
      modify $ \env -> env {envState = "STRING"}
      pure mempty
