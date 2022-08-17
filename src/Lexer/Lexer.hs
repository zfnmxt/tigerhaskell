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

type Action = (String -> Loc -> [Token])

type ActionMap = M.Map EdgeLabel Action

type NodeEnv = Env Char Int (Loc, ActionMap)

type NodeM = RWST DFA () NodeEnv (Either String)

data Node = Node
  { nodeLabel :: Label,
    nodeDelta :: F.Fin Maybe (EdgeLabel, Label) Label,
    nodeDFA :: DFA,
    nodeActions :: ActionMap
  }

type LexEnv = Env Char Label (Loc, Int)

type LexM = RWST Lexer [Token] LexEnv (Either String)

type Lexer = T.DFA EdgeLabel Label (DFA, ActionMap)

mkEdge ::
  Label ->
  Priority ->
  (Label, T.NFA Char Int Label, Action, Label) ->
  (F.Fin Maybe (EdgeLabel, Label) Label, NFA, ActionMap)
mkEdge label priority (edgeLabel, nfa, action, next) =
  (d, nfa {payloads = payloads'}, actions)
  where
    d = F.singleton ((priority, edgeLabel), label) next
    payloads' =
      M.fromList $
        map (\s -> (s, (priority, edgeLabel))) $
          S.toList $
            accept nfa
    actions = M.singleton (priority, edgeLabel) action

mkNode :: Label -> [(Label, T.NFA Char Int Label, Action, Label)] -> Node
mkNode label xs = squash $ zipWith (mkEdge label) [0 ..] xs
  where
    squash edges =
      let (fs, nfas, ms) = unzip3 edges
       in Node
            { nodeLabel = label,
              nodeDelta = mconcat fs,
              nodeDFA = FA.toDFA $ FA.unions nfas,
              nodeActions = mconcat ms
            }

commentNode :: Node
commentNode =
  mkNode
    "COMMENT"
    [ ("*/", R.toNFA $ R.lit "*/", (const . const) mempty, "START"),
      (".", FA.plus $ FA.oneOf lexerAlphabet, (const . const) mempty, "COMMENT")
    ]

startNode :: Node
startNode =
  mkNode
    "START"
    $ ( map
          (\(bt, s) -> (show bt, R.toNFA $ R.lit s, \_ l -> [Token bt l], "START"))
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
      ++ [ ("int", FA.plus $ FA.oneOf digits, \s l -> [Token (INT $ read s) l], "START"),
           ( "id",
             FA.oneOf letters `FA.concat` FA.plus (FA.oneOf $ concat [letters, digits, "_"]),
             \s l -> [Token (ID s) l],
             "START"
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
             \s l -> [Token (STRING $ read s) l],
             "START"
           ),
           ("/*", R.toNFA $ R.lit "/*", (const . const) mempty, "COMMENT")
         ]

lexer :: Lexer
lexer =
  FA
    { delta = mconcat deltas,
      delta_e = mempty,
      start = head labels,
      accept = S.fromList labels,
      states = S.fromList labels,
      alphabet = S.fromList $ M.keys $ mconcat $ map snd dfasActions,
      payloads = M.fromList $ zip labels dfasActions
    }
  where
    labels = map nodeLabel nodes
    deltas = map nodeDelta nodes
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

lookupAction :: NodeM Action
lookupAction = do
  actions <- gets (snd . envMisc)
  edgeLabel <- lookupEdge
  pure $ actions M.! edgeLabel

runNodeM :: NodeM a -> DFA -> NodeEnv -> Either String (a, NodeEnv)
runNodeM m dfa nenv = do
  (a, s, _) <- runRWST m dfa nenv
  pure (a, s)

instance MonadDFA NodeM Char Char Int where
  faLook = do
    rest <- gets envRest
    case rest of
      [] -> pure Nothing
      (c : cs) -> do
        pure $ Just c

  faNext = do
    mc <- FA.faLook
    case mc of
      Nothing -> pure ()
      Just c ->
        modify $ \env ->
          env
            { envConsumed = envConsumed env ++ [c],
              envRest = tail $ envRest env
            }

  faOnStep a =
    modify $ \env ->
      env
        { envMisc =
            let (loc, actions) = envMisc env
             in (updateLoc loc a, actions)
        }

updateLoc :: Loc -> Char -> Loc
updateLoc loc '\n' =
  Loc
    { locLine = locLine loc + 1,
      locCol = 0
    }
updateLoc loc _ = loc {locCol = locCol loc + 1}

runNode :: NodeM ([Token], EdgeLabel)
runNode = FA.faUntil accept reject
  where
    onStep a _ _ =
      modify $ \env ->
        env
          { envMisc =
              let (loc, actions) = envMisc env
               in (updateLoc loc a, actions)
          }
    accept = do
      env@(Env consumed rest state (loc, actions)) <- get
      f <- lookupAction
      edgeLabel <- lookupEdge
      pure (f (envConsumed env) loc, edgeLabel)
    reject =
      throwError "No lex."

instance MonadDFA LexM Char EdgeLabel Label where
  faLook = do
    Env _ rest label (loc, commentDepth) <- get
    case rest of
      "" -> do
        tell [Token EOF loc]
        pure Nothing
      as -> do
        let (dfa, actions) = lexer `FA.lookupPayload` label
            nEnv =
              Env
                { envConsumed = mempty,
                  envRest = rest,
                  envState = start dfa,
                  envMisc = (loc, actions)
                }
        ((token, edgeLabel), nEnv') <- lift $ runNodeM runNode dfa nEnv
        tell token
        modify
          ( \env' ->
              env'
                { envConsumed = envConsumed env' ++ envConsumed nEnv',
                  envRest = envRest nEnv',
                  envMisc =
                    let (_, cd) = envMisc env'
                     in (fst $ envMisc $ nEnv', cd)
                }
          )
        pure $ Just edgeLabel
  faNext = pure ()
  faOnStep _ = pure ()

lex :: LexM ()
lex = FA.faUntil accepts rejects
  where
    accepts = pure ()
    rejects = throwError "No lex."

doLex :: String -> Either String [Token]
doLex = fmap snd . runLexM lex

runLexM :: LexM a -> String -> Either String (a, [Token])
runLexM m s =
  evalRWST m lexer $
    Env
      { envConsumed = mempty,
        envRest = s,
        envState = start lexer,
        envMisc = (initLoc, 0)
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
