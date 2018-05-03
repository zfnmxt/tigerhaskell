-- Inspired by ReadP
module DumbParser (
    Parser
  , satisfy
  , reject
  , char
  , string
  , option
  , choice
  , between
  , munch
  , munch1
  , (<|>)
  , many
  , some
  , empty
  , whitespace
  , eof
  , num
  , chainr1
  , chainl1
  , token
  , sepBy1
) where

import Control.Applicative ( Alternative, (<|>), empty, many, some)
import Data.Char (isSpace, isNumber)
import Control.Monad (void)

data Env = Env { linenum :: Int, colnum :: Int }
type Error = [(Env, String)]

newtype Parser e a = Parser { runParser :: Env -> String -> Either e (a, (Env, String)) }

instance Functor (Parser e) where
  fmap f (Parser p) = Parser $ \env s -> (\(a, es) -> (f a, es)) <$> p env s

instance Applicative (Parser e) where
  pure a = Parser $ \env s -> Right (a, (env, s))
  (Parser f) <*> (Parser p) = Parser $ \env s ->
    case (f env s) of
      Left e -> Left e
      Right (f', (env', s')) -> (\(a, (env'', s'')) -> (f' a, (env'', s''))) <$> (p env' s')

instance Monad (Parser e) where
  return = pure
  (Parser p) >>= f = Parser $ \env s -> do
    (a, (env', s')) <- p env s
    runParser (f a) env' s'

instance Monoid e => Alternative (Parser e) where
  empty = Parser $ \_ _ -> Left mempty
  (Parser p) <|> (Parser q) = Parser $ \env s ->
    case p env s of
      Right x -> Right x
      Left _  -> q env s

reject :: Monoid e => Parser e a
reject = empty

getEnv :: Parser e Env
getEnv = Parser $ \env s -> Right (env, (env, s))

putEnv :: Env -> Parser e ()
putEnv env = Parser $ \_ s -> Right ((), (env, s))

getString :: Parser e String
getString = Parser $ \env s -> Right (s, (env, s))

getNextChar :: Monoid e => Parser e Char
getNextChar = Parser $ \env s ->
  case s of
    ""     -> Left mempty
    (c:cs) -> Right (c, (env', cs))
      where env' = if c == '\n'
                   then
                     env {linenum = linenum env + 1, colnum = 0}
                   else
                     env {colnum = colnum env + 1}

satisfy :: Monoid e => (Char -> Bool) -> Parser e Char
satisfy p = do
  c <- getNextChar
  if p c then return c else reject

char :: Monoid e => Char -> Parser e Char
char c = satisfy (== c)

string :: Monoid e => String -> Parser e String
string s = mapM char s

option :: Monoid e => Parser e a -> a -> Parser e a
option p x = p <|> return x

choice :: Monoid e => [Parser e a] -> Parser e a
choice = foldl (<|>) empty

between :: Monoid e => Parser e open -> Parser e close -> Parser e a -> Parser e a
between open close p = do
  open
  res <- p
  close
  return res

munch :: Monoid e => (Char -> Bool) -> Parser e String
munch p = do
  cs <- getString
  case cs of
    ""     -> return ""
    (c:_) ->
      if p c
        then do
        getNextChar
        rest <- munch p
        return $ c:rest
      else
        return ""

munch1 :: Monoid e => (Char -> Bool) -> Parser e String
munch1 p = do
 c    <- satisfy p
 rest <- munch p
 return $ c:rest

whitespace :: Monoid e => Parser e ()
whitespace = void $ munch isSpace

eof :: Monoid e => Parser e ()
eof = do
  s <- getString
  if s == [] then return () else reject

num :: Monoid e => Parser e Int
num = do
  s <- munch1 isNumber
  return $ read s

chainr1 :: Monoid e => Parser e a -> Parser e (a -> a -> a) -> Parser e a
chainr1 p op =  p <|> do
  l <- p
  op' <- op
  r <- chainr1 p op
  return $ l `op'` r

chainl1 :: Monoid e => Parser e a -> Parser e (a -> a -> a) -> Parser e a
chainl1 p op = p >>= f
  where
    f l = return l <|> do
      op' <- op
      r   <- p
      f (l `op'` r)

token :: Monoid e => Parser e a -> Parser e a
token p = whitespace >> p

sepBy1 :: Monoid e => Parser e a -> Parser e sep -> Parser e [a]
sepBy1 p sep = do
  first <- p
  rest  <- many (sep >> p)
  return $ first:rest
