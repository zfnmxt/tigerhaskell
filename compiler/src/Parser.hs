module Parser where

import DumbParser
import TigerDef

ident :: Monoid e => Parser e String
ident = do
  x <- letter
  rest <- many $ choice [letter, digit, char '_']
  return $ x:rest

comment :: Monoid e => Parser e ()
comment = between (string "/*") (string "*/") body
  where body = do
          many $ satisfy (`notElem` ['*', '/'])
          choice [ comment >> body
                 , ((char '*') >> satisfy (/= '/') >> body)
                 , return ()
                 ]
