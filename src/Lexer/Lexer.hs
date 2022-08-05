module Lexer.Lexer where

import Control.Monad.Except
import Control.Monad.State
import Data.List (nub)
import Data.Maybe
import qualified Data.Set as S
import qualified Lexer.FA as FA
import Lexer.Regex
import Lexer.Tokens
import Lexer.Types

data ActionBase a = ActionBase
  { action :: a,
    actionFun :: String -> Loc -> Token,
    actionState :: [String]
  }

type Action = ActionBase (NFA Char String)

data Env = Env
  { envText :: String,
    envCur :: String,
    envLoc :: Loc,
    envActions :: [Action]
  }

type LexM = StateT Env (Either String)

runLexM :: LexM a -> String -> Either String a
runLexM m s = evalStateT m (Env s mempty initLoc actions)

runActions :: Char -> [Action] -> [Action]
runActions x = mapMaybe $ \a ->
  case nub $ Prelude.concat [FA.step (action a) x s | s <- actionState a] of
    [] -> Nothing
    ss' -> pure $ a {actionState = ss'}

nextChar :: LexM ()
nextChar = do
  ss <- gets envText
  case ss of
    [] -> throwError "End of input."
    (s : ss') -> do
      modify
        ( \env ->
            env
              { envText = ss',
                envCur = envCur env ++ [s]
              }
        )
      pure ()

look :: LexM (Maybe Char)
look = do
  ss <- gets envText
  case ss of
    [] -> pure Nothing
    (s : _) -> pure $ Just s

nextToken :: LexM (Token)
nextToken = loop
  where
    loop = do
      as <- gets envActions
      mc <- look
      case mc of
        Nothing -> Token EOF <$> gets envLoc
        Just c -> do
          let as' = runActions c as
          case (as, as') of
            ([], _) -> throwError "No success.1"
            ([a], [])
              | any (`S.member` accept (action a)) (actionState a) -> do
                  s <- gets envCur
                  loc <- gets envLoc
                  pure $ actionFun a s loc
              | otherwise -> throwError "No success.2"
            _ -> do
              nextChar
              modify $ \env ->
                env
                  { envActions = as'
                  }
              loop

actions :: [Action]
actions =
  map
    actionfy
    [ ActionBase
        { action = digit,
          actionFun = \s -> Token (INT $ read s),
          actionState = mempty
        }
    ]
  where
    actionfy (ActionBase r f _) =
      let nfa = toNFA r
       in ActionBase nfa f [start nfa]
