module Lexer.Lexer where

import Control.Monad.Except
import Control.Monad.RWS
import Data.Char
import Data.List (nub)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Debug.Trace
import qualified Lexer.FA as FA
import qualified Lexer.Finite as F
import qualified Lexer.Regex as R
import Lexer.Tokens
import Lexer.Types hiding (DFA, NFA)
import qualified Lexer.Types as T
import Prelude hiding (lex)

data Env = Env
  { envInput :: String,
    envLoc :: Loc,
    envCommentDepth :: Int,
    envLabel :: Label
  }
  deriving (Show)

data NEnv = NEnv
  { nEnvConsumed :: String,
    nEnvRest :: String,
    nEnvLoc :: Loc,
    nEnvState :: Int,
    nEnvDFA :: DFA,
    nEnvActions :: M.Map EdgeLabel (String -> Loc -> [Token])
  }

type NodeM = RWST () () NEnv (Either String)

type Label = String

type Priority = Int

type EdgeLabel = (Priority, String)

type LexM = RWST () [Token] Env (Either String)

type Lexer = T.DFA EdgeLabel Label (DFA, M.Map EdgeLabel (String -> Loc -> [Token]))

type DFA = T.DFA Char Int EdgeLabel

type NFA = T.NFA Char Int EdgeLabel

type Node = (Label, F.Fin Maybe (EdgeLabel, Label) Label, (DFA, M.Map EdgeLabel (String -> Loc -> [Token])))

mkNode :: Label -> [(Label, T.NFA Char Int Label, String -> Loc -> [Token], Label)] -> Node
mkNode label xs = squash $ zipWith (edge label) [0 ..] xs
  where
    squash edges =
      let (fs, nfams) = unzip edges
          (nfas, ms) = unzip nfams
       in (label, mconcat fs, (toDFA $ FA.unions nfas, mconcat ms))

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
    [ ("if", R.toNFA $ R.lit "if", \s l -> [mkToken "IF" s l], "START"),
      ("[a-z]+", FA.plus $ FA.oneOf ['a' .. 'z'], \s l -> [mkToken "ID" s l], "START"),
      ("/*", R.toNFA $ R.lit "/*", (const . const) mempty, "COMMENT")
    ]

edge ::
  Label ->
  Priority ->
  (Label, T.NFA Char Int Label, String -> Loc -> [Token], Label) ->
  (F.Fin Maybe (EdgeLabel, Label) Label, (NFA, M.Map EdgeLabel (String -> Loc -> [Token])))
edge label priority (edgeLabel, nfa, action, next) =
  ( F.singleton ((priority, edgeLabel), label) next,
    (nfa {payloads = M.fromList $ map (\s -> (s, (priority, edgeLabel))) $ S.toList $ accept nfa}, M.singleton (priority, edgeLabel) action)
  )

lexer :: Lexer
lexer =
  FA
    { delta = mconcat delta',
      delta_e = mempty,
      start = head states',
      accept = S.fromList states',
      states = S.fromList states',
      alphabet = S.fromList $ M.keys $ mconcat $ map snd payloads', -- shouldn't concat
      payloads = M.fromList $ zip states' payloads'
    }
  where
    (states', delta', payloads') =
      unzip3
        [ startNode,
          commentNode
        ]

lookupEdge :: NodeM EdgeLabel
lookupEdge = do
  dfa <- gets nEnvDFA
  state <- gets nEnvState
  pure $ dfa `FA.lookupPayload` state

lookupAction :: NodeM (String -> Loc -> [Token])
lookupAction = do
  actions <- gets nEnvActions
  edgeLabel <- lookupEdge
  pure $ actions M.! edgeLabel

runNodeM :: NodeM a -> NEnv -> Either String (a, NEnv)
runNodeM m nenv = do
  (a, s, _) <- runRWST m () nenv
  pure (a, s)

runNode :: NodeM ([Token], EdgeLabel)
runNode = do
  nEnv@(NEnv consumed rest loc state dfa actions) <- get
  case rest of
    "" -> do
      unless (state `S.member` accept dfa) $
        throwError "No lex1."

      f <- lookupAction
      edgeLabel <- lookupEdge
      pure (f consumed loc, edgeLabel)
    (c : cs) -> do
      case FA.step dfa c state of
        Nothing -> do
          unless (state `S.member` accept dfa) $
            throwError "No lex2."

          unless (not $ null consumed) $
            throwError "No lex3."

          f <- lookupAction
          edgeLabel <- lookupEdge
          pure (f consumed loc, edgeLabel)
        Just s' -> do
          put
            nEnv
              { nEnvConsumed = consumed ++ [c],
                nEnvRest = cs,
                nEnvLoc = updateLoc loc c,
                nEnvState = s'
              }
          runNode
  where
    updateLoc :: Loc -> Char -> Loc
    updateLoc loc '\n' =
      Loc
        { locLine = locLine loc + 1,
          locCol = 0
        }
    updateLoc loc _ = loc {locCol = locCol loc + 1}

lex :: LexM ()
lex = do
  env@(Env input loc commentDepth label) <- get
  case input of
    "" -> tell [Token EOF loc]
    cs -> do
      let (dfa, actions) = lexer `FA.lookupPayload` label
          nEnv =
            NEnv
              { nEnvConsumed = mempty,
                nEnvRest = input,
                nEnvLoc = loc,
                nEnvState = start dfa,
                nEnvDFA = dfa,
                nEnvActions = actions
              }
      ((token, edgeLabel), nEnv') <- lift $ runNodeM runNode nEnv
      tell token
      let label' = fromMaybe (error "") $ FA.step lexer edgeLabel label
      -- traceM $ "token: " <> show token
      modify
        ( \env' ->
            env'
              { envInput = nEnvRest nEnv',
                envLoc = nEnvLoc nEnv',
                envLabel = label'
              }
        )
      env' <- get
      lex

doLex :: String -> Either String [Token]
doLex = fmap snd . runLexM lex

runLexM :: LexM a -> String -> Either String (a, [Token])
runLexM m s =
  evalRWST m () $
    Env
      { envInput = s,
        envLoc = initLoc,
        envCommentDepth = 0,
        envLabel = start lexer
      }

class RegLang r where
  toNFA :: r -> NFA
  toDFA :: r -> DFA

instance RegLang (Regex Char) where
  toNFA = R.toNFA
  toDFA = R.toDFA

instance RegLang NFA where
  toNFA = id
  toDFA = FA.toDFA

-- class Labeled a where
--  label :: String -> a -> NFA
--  ignore :: a -> NFA Char Int String
--
-- instance Labeled (Regex Char) where
--  label name r = nfa {payloads = M.fromList $ map (\s -> (s, name)) $ S.toList $ accept nfa}
--    where
--      nfa = R.toNFA r
--  ignore r = (R.toNFA r) {payloads = M.empty}
--
-- instance Labeled (NFA Char Int String) where
--  label name nfa = nfa {payloads = M.fromList $ map (\s -> (s, name)) $ S.toList $ accept nfa}
--  ignore nfa = nfa {payloads = M.empty}
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

whitespace :: Regex Char
whitespace = Star $ R.oneOf [' ', '\t', '\n']

digits :: [Char]
digits = ['0' .. '9']

alphabet :: [Char]
alphabet = ['a' .. 'z']

letters :: [Char]
letters = filter isLetter [minBound .. maxBound]

printable :: [Char]
printable = filter isPrint [minBound .. maxBound]

lexerAlphabet :: [Char]
lexerAlphabet = ['a' .. 'z'] ++ ['A' .. 'Z'] -- printable

-- action :: String -> Action
-- action "COMMENT_START" _ _ = do
--  modify $ \env -> env { envCommentDepth = envCommentDepth env + 1 }
--
-- action lab s l = do
--  let token = mkToken lab s l
--  case
