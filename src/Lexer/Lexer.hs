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
    lexCommentDepth :: Int
  }

type LexEnv = Env Char Label LexEnvMisc

type LexM = RWST Lexer [Token] LexEnv (Either String)

newtype Action = Action {runAction :: String -> Loc -> LexM [Token]}

type ActionMap = M.Map EdgeLabel Action

type Lexer = T.DFA EdgeLabel Label (DFA, ActionMap)

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
      dropCommentDepth
      commentDepth <- gets (lexCommentDepth . envMisc)
      let s'
            | commentDepth > 0 = "COMMENT"
            | commentDepth < 0 = error "oops"
            | otherwise = "START"
      modify $ \env -> env {envState = s'}
      pure mempty
    start_action = Action $ \s l -> do
      bumpCommentDepth
      pure mempty

bumpCommentDepth :: LexM ()
bumpCommentDepth =
  modify $ \env ->
    env
      { envMisc =
          let lexMisc = envMisc env
           in lexMisc {lexCommentDepth = lexCommentDepth lexMisc + 1}
      }

dropCommentDepth :: LexM ()
dropCommentDepth =
  modify $ \env ->
    env
      { envMisc =
          let lexMisc = envMisc env
           in lexMisc {lexCommentDepth = lexCommentDepth lexMisc - 1}
      }

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
           ( "string",
             R.toNFA (Sym '\"')
               `FA.concat` ( FA.plus $
                               FA.oneOf (' ' : (printable L.\\ ['\\', '\"']))
                                 `FA.union` R.toNFA
                                   ( Sym '\\'
                                       ::: R.unions
                                         ( [ R.plus (R.oneOf [' ', '\t', '\n', '\f'] ::: Sym '\\'),
                                             (Sym '^' ::: R.oneOf ['A' .. 'Z'])
                                           ]
                                             ++ map R.lit ["n", "t", "ddd", "\"", "\\"]
                                         )
                                   )
                           )
               `FA.concat` R.toNFA (Sym '\"'),
             Action $ \s l -> pure [Token (STRING $ read s) l]
           ),
           ("/*", R.toNFA $ R.lit "/*", comment_start_action)
         ]
  where
    comment_start_action = Action $ \s l -> do
      bumpCommentDepth
      modify $ \env -> env {envState = "COMMENT"}
      pure mempty

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
        commentNode
      ]

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

runNodeM :: NodeM a -> DFA -> NodeEnv -> Either String (a, NodeEnv)
runNodeM m dfa nenv = do
  (a, s, _) <- runRWST m dfa nenv
  pure (a, s)

runNode :: NodeM EdgeLabel
runNode = FA.run onStep accept reject
  where
    onStep c = modify $ envUpdLoc $ updateLoc c
    accept = lookupEdge
    reject =
      throwError "No lex."

lex :: LexM ()
lex = do
  Env _ rest label (LexEnvMisc loc commentDepth) <- get
  case rest of
    "" -> tell [Token EOF loc]
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
      token <- runAction action (envConsumed nEnv') (envLoc nEnv')
      tell token
      modify
        ( \env' ->
            envUpdLoc (const $ envLoc nEnv') $
              env'
                { envConsumed = envConsumed env' ++ envConsumed nEnv',
                  envRest = envRest nEnv'
                }
        )
      lex

doLex :: String -> Either String [Token]
doLex = fmap snd . runLexM lex

runLexM :: LexM a -> String -> Either String (a, [Token])
runLexM m s =
  evalRWST m lexer $
    Env
      { envConsumed = mempty,
        envRest = s,
        envState = start lexer,
        envMisc =
          LexEnvMisc
            { lexLoc = initLoc,
              lexCommentDepth = 0
            }
      }

-- instance Labeled (Regex Char) where
-- label name r =
--
--   nfa {payloads = M.fromList $ map (\s -> (s, name)) $ S.toList $ accept nfa}
--   where
--     nfa = R.toNFA r
-- ignore r = (R.toNFA r) {payloads = M.empty}
--
-- instance Labeled (NFA Char Int String) where
-- label name nfa = nfa {payloads = M.fromList $ map (\s -> (s, name)) $ S.toList $ accept nfa}
-- ignore nfa = nfa {payloads = M.empty}
--
-- lexerDFA :: DFA Char Int String
-- lexerDFA =
--  toDFA $
--    FA.unions
--      [ label "TYPE" $ R.lit "type",
--        label "VAR" $ R.lit "var",
--        label "FUNCTION" $ R.lit "function",
--        label "BREAK" $ R.lit "break",
--        label "OF" $ R.lit "of",
--        label "END" $ R.lit "end",
--        label "IN" $ R.lit "in",
--        label "NIL" $ R.lit "nil",
--        label "LET" $ R.lit "let",
--        label "DO" $ R.lit "do",
--        label "TO" $ R.lit "to",
--        label "FOR" $ R.lit "for",
--        label "WHILE" $ R.lit "while",
--        label "ELSE" $ R.lit "else",
--        label "THEN" $ R.lit "then",
--        label "IF" $ R.lit "if",
--        label "ARRAY" $ R.lit "array",
--        label "ASSIGN" $ R.lit ":=",
--        label "OR" $ R.lit "|",
--        label "AND" $ R.lit "&",
--        label "GE" $ R.lit ">=",
--        label "GT" $ R.lit ">",
--        label "LE" $ R.lit "<=",
--        label "LT" $ R.lit "<",
--        label "NEQ" $ R.lit "<>",
--        label "EQ" $ R.lit "=",
--        label "DIVIDE" $ R.lit "/",
--        label "TIMES" $ R.lit "*",
--        label "MINUS" $ R.lit "-",
--        label "PLUS" $ R.lit "+",
--        label "DOT" $ R.lit ".",
--        label "RBRACE" $ R.lit "}",
--        label "LBRACE" $ R.lit "{",
--        label "RBRACK" $ R.lit "]",
--        label "LBRACK" $ R.lit "[",
--        label "RPAREN" $ R.lit ")",
--        label "LPAREN" $ R.lit "(",
--        label "SEMICOLON" $ R.lit ";",
--        label "COLON" $ R.lit ":",
--        label "COMMA" $ R.lit ",",
--        label "ID" $ FA.oneOf letters `FA.concat` FA.star (FA.oneOf $ letters ++ digits ++ "_"),
--        label "INT" $ FA.plus $ FA.oneOf digits,
--        label "COMMENT_START" $ R.lit "/*",
--        label "COMMENT_END" $ R.lit "*/",
--        ignore whitespace
--      ]
--
-- commentDFA :: DFA Char Int String
-- commentDFA =
--  toDFA $
--    FA.unions
--      [ label "COMMENT_BODY" $ FA.oneOf $ printable
--      ]

digits :: [Char]
digits = ['0' .. '9']

alphabet :: [Char]
alphabet = ['a' .. 'z']

letters :: [Char]
letters = ['A' .. 'Z'] ++ ['a' .. 'z']

-- letters = filter isLetter [minBound .. maxBound]

printable :: [Char]
printable = letters ++ digits

-- printable = filter isPrint [minBound .. maxBound]

lexerAlphabet :: [Char]
lexerAlphabet = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ [' ', '\n', '\t'] ++ ['0' .. '9'] -- printable

-- action :: String -> Action
-- action "COMMENT_START" _ _ = do
--  modify $ \env -> env { envCommentDepth = envCommentDepth env + 1 }
--
-- action lab s l = do
--  let token = mkToken lab s l
--  case
