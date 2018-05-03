module DumbParser where

import Control.Applicative ( Alternative, (<|>), empty, many, some)
import Data.Char (isSpace)
import Control.Monad (void)

newtype Parser a = Parser { runParser :: String -> [(a, String)] }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> (\(a, b) -> (f a, b)) <$> p s

instance Applicative Parser where
  pure a = Parser $ \s -> [(a, s)]
  (Parser f) <*> (Parser p) = Parser $ \s -> [ (f a, s'') | (f, s') <- f s,  (a, s'') <- p s']

instance Monad Parser where
  return = pure
  (Parser p) >>= f = Parser $ \s -> concat $ map (\(a, s') -> runParser (f a) s') (p s)

instance Alternative Parser where
  empty = failed
  (Parser p) <|> (Parser q) = Parser $ \s -> (p s) ++ (q s)

remaining :: Parser String
remaining = Parser $ \s -> [(s,s)]

getNextChar :: Parser Char
getNextChar = Parser $ \s ->
  case s of
    ""     -> []
    (c:cs) -> [(c,cs)]

failed :: Parser a
failed = Parser $ \s -> []

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  c <- getNextChar
  if p c then return c else failed

char :: Char -> Parser Char
char c = satisfy (== c)

string :: String -> Parser String
string s = mapM char s

option :: a -> Parser a -> Parser a
option x p = return x <|> p

choice :: [Parser a] -> Parser a
choice = foldl (<|>) empty

between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = do
  open
  res <- p
  close
  return res

munch :: (Char -> Bool) -> Parser String
munch p = do
  (c:cs) <- remaining
  if p c
    then do
      getNextChar
      rest <- munch p
      return $ c:rest
    else
      return ""

munch1 :: (Char -> Bool) -> Parser String
munch1 p = do
 c    <- satisfy p
 rest <- munch p
 return $ c:rest

whitespace :: Parser ()
whitespace = void $ munch isSpace

eof :: Parser ()
eof = do
  s <- remaining
  if s == [] then return () else failed

chainr :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainr p op x = undefined

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op x = undefined


