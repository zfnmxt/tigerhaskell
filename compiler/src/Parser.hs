{-# LANGUAGE FlexibleInstances #-}

module Parser where

import DumbParser
import TigerDef
import Data.Char (chr)

type TigerError = String
type TigerP = Parser TigerError

instance Error TigerError where
  errEmpty = ""
  errFail env = "Parser failure at " ++ line ++ ":" ++ col
    where line = show $ linenum env
          col  = show $ colnum env

ident :: TigerP String
ident = do
  x <- letter
  rest <- many $ choice [letter, digit, char '_']
  return $ x:rest

comment :: TigerP ()
comment = between (string "/*") (string "*/") body
  where body = void $ manyTo (comment <|> throw)  "*/"

stringLit :: TigerP String
stringLit = concat <$> (many $ escape <|> listify alpha <|> listify (char ' '))

-- Note: doesn't support control characters
escapeChar :: TigerP Char
escapeChar = choice [ string "\\n" >> return '\n'
                    , string "\\t" >> return '\t'
                    , string "\""  >> return '"'
                    , string "\\"  >> return '\\'
                    ]

escape :: TigerP String
escape = do
  choice $ [ multiline
           , listify escapeChar
           , ascii
           ]
    where ascii      = char '\\' >> parseN 3 digit >>= \s -> return $ [chr (read s)]
          multiline  = between (char '\\') (char '\\') whitespace >> return ""

integerLit :: TigerP Int
integerLit = num

noVal :: TigerP ()
noVal = between (char '(') (char ')') $ return ()
