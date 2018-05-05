module TigerDef where

keywords :: [String]
keywords = [ "while", "for", "to", "break", "let", "in", "end", "function"
           , "var", "type", "array", "if", "then", "else", "do", "of", "nil"
           ]

punctuation :: [String]
punctuation = [ ",", ":", ";", "(", ")", "[", "]", "{", "}", ".", "+"
              , "-", "*", "/", "=", "<>", "<", "<=", ">", ">=", "&", "|", ":="
              ]

letters :: [Char]
letters = ['a'..'z'] ++ ['A'..'Z']
