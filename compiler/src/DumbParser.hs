-- Inspired by ReadP
module DumbParser (
    Parser
  , Error (..)
  , Env (..)
  , satisfy
  , reject
  , maybeP
  , char
  , string
  , option
  , choice
  , between
  , parseN
  , void
  , (<|>)
  , many
  , many1
  , throw
  , some
  , empty
  , whitespace
  , eof
  , number
  , digit
  , letter
  , alpha
  , chainr1
  , chainl1
  , token
  , sepBy1
  , sepBy
  , runParser
  , manyTo
  , listify
  , stoken
) where

import Control.Applicative ( Alternative, (<|>), empty, many, some)
import Data.Char (isSpace, isNumber, isDigit, isAlpha)
import Data.List (isPrefixOf)
import Control.Monad (void)

data Env = Env { linenum :: Int, colnum :: Int } deriving (Show)

class Error e where
  errEmpty :: e
  errFail  :: Env -> e

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

instance Error e => Alternative (Parser e) where
  empty = Parser $ \_ _ -> Left errEmpty
  (Parser p) <|> (Parser q) = Parser $ \env s ->
    case p env s of
      Right x -> Right x
      Left _  -> q env s

reject :: Error e => Parser e a
reject = Parser $ \env s -> Left $ errFail env

getEnv :: Parser e Env
getEnv = Parser $ \env s -> Right (env, (env, s))

putEnv :: Env -> Parser e ()
putEnv env = Parser $ \_ s -> Right ((), (env, s))

getString :: Parser e String
getString = Parser $ \env s -> Right (s, (env, s))

getNextChar :: Error e => Parser e Char
getNextChar = Parser $ \env s ->
  case s of
    ""     -> Left errEmpty
    (c:cs) -> Right (c, (env', cs))
      where env' = if c == '\n'
                   then
                     env {linenum = linenum env + 1, colnum = 0}
                   else
                     env {colnum = colnum env + 1}

satisfy :: Error e => (Char -> Bool) -> Parser e Char
satisfy p = do
  c <- getNextChar
  if p c then return c else reject

char :: Error e => Char -> Parser e Char
char c = satisfy (== c)

string :: Error e => String -> Parser e String
string s = mapM char s

option :: Error e => Parser e a -> a -> Parser e a
option p x = p <|> return x

choice :: Error e => [Parser e a] -> Parser e a
choice = foldr (<|>) empty

between :: Error e => Parser e open -> Parser e close -> Parser e a -> Parser e a
between open close p = do
  open
  res <- p
  close
  return res

many1 :: Error e => Parser e a -> Parser e [a]
many1 p = do
 first <- p
 rest  <- many p
 return $ first:rest

parseN :: Error e => Int -> Parser e a -> Parser e [a]
parseN n p = mapM (\_ -> p) [1..n]

whitespace :: Error e => Parser e ()
whitespace = void $ many (satisfy isSpace)

eof :: Error e => Parser e ()
eof = do
  s <- getString
  if s == [] then return () else reject

number :: Error e => Parser e Integer
number = do
  s <- munch1 isNumber
  return $ read s

digit :: Error e => Parser e Char
digit = satisfy isDigit

letter :: Error e => Parser e Char
letter = satisfy (`elem` (['a'..'z'] ++ ['A'..'Z']))

alpha :: Error e => Parser e Char
alpha = satisfy isAlpha

chainr1 :: Error e => Parser e a -> Parser e (a -> a -> a) -> Parser e a
chainr1 p op =  p <|> do
  l <- p
  op' <- op
  r <- chainr1 p op
  return $ l `op'` r

chainl1 :: Error e => Parser e a -> Parser e (a -> a -> a) -> Parser e a
chainl1 p op = p >>= f
  where
    f l = return l <|> do
      op' <- op
      r   <- p
      f (l `op'` r)

token :: Error e => Parser e a -> Parser e a
token p = whitespace >> p

stoken :: Error e => Parser e a -> Parser e a
stoken p = munch1 (== ' ')  >> p

sepBy1 :: Error e => Parser e a -> Parser e sep -> Parser e [a]
sepBy1 p sep = do
  first <- p
  rest  <- many (sep >> p)
  return $ first:rest

sepBy :: Error e => Parser e a -> Parser e sep -> Parser e [a]
sepBy p sep = sepBy1 p sep <|> return []

throw :: Error e => Parser e ()
throw = getNextChar >> return ()

manyTo :: Error e => Parser e a -> String -> Parser e [a]
manyTo p s = do
  s' <- getString
  if s `isPrefixOf` s'
    then
    return []
    else do
    next <- p
    rest <- manyTo p s
    return $ next:rest

listify :: Error e => Parser e a -> Parser e [a]
listify p = (\a -> [a]) <$> p

maybeP :: Error e => Parser e a -> Parser e (Maybe a)
maybeP p = Just <$> p <|> return Nothing

