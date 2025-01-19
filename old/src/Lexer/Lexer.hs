{-# LANGUAGE TupleSections #-}

module Lexer.Lexer where

import Control.Monad.Except
import Control.Monad.RWS
import Data.Char
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Lexer.FA as FA
import qualified Lexer.Regex as R
import Lexer.Tokens
import Lexer.Types hiding (DFA, NFA)
import qualified Lexer.Types as T
import Prelude hiding (EQ, GT, LT, lex)

data Env a s = Env
  { envConsumed :: [a],
    envRest :: [a],
    envState :: s,
    envLoc :: Loc,
    envCommentDepth :: Int,
    envInString :: Bool
  }

type Label = String

type Priority = Int

type EdgeLabel = (Priority, String)

type DFA = T.DFA Char Int EdgeLabel

type NFA = T.NFA Char Int EdgeLabel

data Node = Node
  { nodeLabel :: Label,
    nodeDFA :: DFA,
    nodeActions :: ActionMap
  }

type LexM = RWST Lexer [Token] (Env Char Label) (Either String)

newtype Action = Action {runAction :: String -> Loc -> LexM [Token]}

type ActionMap = M.Map EdgeLabel Action

type Lexer = T.DFA EdgeLabel Label (DFA, ActionMap)

lexError :: String -> LexM a
lexError msg = do
  l <- gets envLoc
  throwError $ "Lexing error:" <> show l <> ": " <> msg

lex :: String -> Either String [Token]
lex = fmap snd . evalLexM runLexer

evalLexM :: LexM a -> String -> Either String (a, [Token])
evalLexM m s =
  evalRWST m lexer $
    Env
      { envConsumed = mempty,
        envRest = s,
        envState = start lexer,
        envLoc = mempty,
        envCommentDepth = 0,
        envInString = False
      }

runLexer :: LexM ()
runLexer = do
  Env _ rest label l commentDepth inString <- get
  case rest of
    "" -> do
      unless (commentDepth == 0) $
        lexError $
          "Unclosed comment: " ++ show commentDepth

      when inString $
        lexError "Unclosed string."

      tell [Token EOF l]
    _ -> do
      let (dfa, _) = lexer `FA.lookupPayload` label
          (s, rest') = FA.greedyIntDFA_ dfa rest
          edgeLabel = dfa `FA.lookupPayload` s
          consumed = take (length rest - length rest') rest
          l' = foldl (flip updateLoc) l consumed

      when (rest' == rest) $
        lexError "Lex error."

      action <- lookupAction edgeLabel
      t <- runAction action consumed l
      tell t
      modify
        ( \env' ->
            env'
              { envConsumed = envConsumed env' ++ consumed,
                envRest = rest',
                envLoc = l'
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

mkEdge ::
  Priority ->
  (Label, T.NFA Char Int Label, Action) ->
  (NFA, ActionMap)
mkEdge priority (edgeLabel, nfa, action) =
  (nfa {payloads = payloads'}, actions)
  where
    payloads' =
      M.fromList $
        map (,(priority, edgeLabel)) $
          S.toList $
            accept nfa
    actions = M.singleton (priority, edgeLabel) action

mkNode :: Label -> [(Label, T.NFA Char Int Label, Action)] -> Node
mkNode label = squash . zipWith mkEdge [0 ..]
  where
    squash edges =
      let (nfas, ms) = unzip edges
       in Node
            { nodeLabel = label,
              nodeDFA = FA.toDFA $ FA.unions nfas,
              nodeActions = mconcat ms
            }

lookupAction :: EdgeLabel -> LexM Action
lookupAction edgeLabel = do
  dfa <- ask
  label <- gets envState
  let (_, actions) = dfa `FA.lookupPayload` label
  pure $ actions M.! edgeLabel

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
                        Sym '^' ::: R.oneOf ['A' .. 'Z']
                      ]
                        ++ map R.lit ["n", "t", "ddd", "\"", "\\"]
                    )
              ),
        Action $ \s l -> pure [Token (STRING $ read $ "\"" ++ s ++ "\"") l]
      )
    ]
  where
    end_action = Action $ \_ _ -> do
      modify $ \env -> env {envState = "START", envInString = False}
      pure mempty

commentNode :: Node
commentNode =
  mkNode
    "COMMENT"
    [ ("*/", R.toNFA $ R.lit "*/", end_action),
      ("/*", R.toNFA $ R.lit "/*", start_action),
      ( ".",
        FA.star
          ( FA.oneOf
              (L.delete '*' lexerAlphabet)
          )
          `FA.union` R.toNFA (Sym '*'),
        Action $ (const . const) (pure mempty)
      )
    ]
  where
    end_action = Action $ \_ _ -> do
      commentDepth <- (\x -> x - 1) <$> gets envCommentDepth
      when (commentDepth < 0) $
        lexError "Missing /*"
      let s'
            | commentDepth > 0 = "COMMENT"
            | otherwise = "START"
      modify $ \env -> env {envState = s', envCommentDepth = commentDepth}
      pure mempty
    start_action = undefined

startNode :: Node
startNode =
  mkNode
    "START"
    $ map
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
      ++ [ ("int", FA.plus $ FA.oneOf digits, Action $ \s l -> pure [Token (INT $ read s) l]),
           ( "id",
             FA.oneOf letters `FA.concat` FA.star (FA.oneOf $ concat [letters, digits, "_"]),
             Action $ \s l -> pure [Token (ID s) l]
           ),
           ("/*", R.toNFA $ R.lit "/*", comment_start_action),
           ("\"", R.toNFA $ Sym '\"', string_start_action),
           ("whitespace", FA.star $ FA.oneOf whitespace, Action $ (const . const) (pure mempty))
         ]
  where
    comment_start_action = Action $ \_ _ -> do
      modify $ \env -> env {envState = "COMMENT", envCommentDepth = envCommentDepth env + 1}
      pure mempty
    string_start_action = Action $ \_ _ -> do
      modify $ \env -> env {envState = "STRING", envInString = True}
      pure mempty

updateLoc :: Char -> Loc -> Loc
updateLoc '\n' l =
  Loc
    { locLine = locLine l + 1,
      locCol = 0
    }
updateLoc _ l = l {locCol = locCol l + 1}

whitespace :: [Char]
whitespace = filter isSpace [minBound .. maxBound]

digits :: [Char]
digits = ['0' .. '9']

-- Using the full character sets requires that the lexer generate an
-- actual lexer module at compiler-time; otherwise it takes too long
-- to build the DFAs.

letters :: [Char]
letters = filter (\c -> isLatin1 c && isLetter c) [minBound .. maxBound]

-- letters = filter isLetter [minBound .. maxBound]

printable :: [Char]
printable = filter isLatin1 [minBound .. maxBound] ++ digits

-- printable = filter isPrint [minBound .. maxBound]

lexerAlphabet :: [Char]
lexerAlphabet = printable ++ whitespace
