module Scan (end, scan)
where

import Data.Char (isSpace, isAlpha, isAlphaNum, isDigit, isOctDigit, isHexDigit,
	     digitToInt, chr)
import Data.List (isPrefixOf)

import Ir
import Token
import Location (Loc (Loc), incc, incl, incchar, incs, merge, advance)

end :: [Tok] -> [Tok]
end []                           = []
end (TokSeq l : TokEnd l' : tokens)   = (TokSeqEnd (merge l l') : end tokens)
end (TokCall l : TokEnd l' : tokens)  = (TokCallEnd (merge l l') : end tokens)
end (TokLabel l : TokEnd l' : tokens) = (TokLabelEnd (merge l l') : end tokens)
end (token : tokens)             = (token : end tokens)

-- Initial part of an identifier: [a-zA-Z$_()+-*/%<>=].
isIdent :: Char -> Bool
isIdent c
  | isAlpha c      = True
  | c `elem` "$_()+-*/%<>=" = True
  | otherwise      = False

-- Part of an identifier: [a-zA-Z0-9$_()+-*/%<>=].
isIdentNum :: Char -> Bool
isIdentNum c
  | isIdent c = True
  | isDigit c = True
  | otherwise = False

scan :: String -> Loc -> [Tok]
scan [] loc = []

-- White spaces
scan ('\n' : cs) loc = scan cs (incl 1 loc)
scan ('\r' : cs) loc = scan cs (incl 1 loc)

-- Comments
scan ('/' : '*' : cs) loc = scanComment "*/" cs (incc 2 loc)
scan ('#' : cs) loc = scanComment "\n" cs (incc 1 loc)

scan ('"' : cs) loc = scanLiteral cs (incc 1 $ advance loc)

-- Negative int
scan ('-' : input@(c : cs)) loc
    | isDigit c = scanTokInt False (c : cs) (incc 1 $ advance loc)

scan input@(c : cs) loc
    | isSpace c = scan cs (incc 1 loc)
    | isDigit c = scanTokInt True input (advance loc)
    | isIdent c = scanTokString input (advance loc)
    | otherwise = error (show loc ++ ": unexpected character: " ++ [c])

scanComment :: String -> String -> Loc -> [Tok]
scanComment close cs loc
  | isPrefixOf close cs = scan (drop (length close) cs) (incs close loc)
  | otherwise           = scanComment close (tail cs) (incchar (head cs) loc)

scanLiteral :: String -> Loc -> [Tok]
scanLiteral input loc =
    case scanLiteralContent input loc of
	(string, loc, tokens) -> TokLiteral (string, loc) : tokens

escapeToChar :: Char -> Char
escapeToChar 'a'  = '\a'
escapeToChar 'b'  = '\b'
escapeToChar 'f'  = '\f'
escapeToChar 'n'  = '\n'
escapeToChar 'r'  = '\r'
escapeToChar 't'  = '\t'
escapeToChar 'v'  = '\v'
escapeToChar '\\' = '\\'
escapeToChar '"'  = '"'
escapeToChar '\'' = '\''

scanLiteralContent :: String -> Loc -> (String, Loc, [Tok])
scanLiteralContent ('\\' : c : cs) loc
    | c `elem` "abfnrtv\\\"'" =
    case scanLiteralContent cs (incc 2 loc) of
	 (string, loc, tokens) -> (escapeToChar c : string, loc, tokens)

scanLiteralContent ('\\' : 'x' : h : l : cs) loc
    | isHexDigit h && isHexDigit l =
	case scanLiteralContent cs (incc 4 loc) of
	     (string, loc, tokens) -> (c : string, loc, tokens)
	    where c = chr ((digitToInt h) * 16 + (digitToInt l))

scanLiteralContent ('\\' : h : m : l : cs) loc
    | isOctDigit h && isOctDigit m && isOctDigit l =
	case scanLiteralContent cs (incc 4 loc) of
	     (string, loc, tokens) -> (c : string, loc, tokens)
	    where c = chr ((digitToInt h) * 64
                           + (digitToInt m) * 8
                           + (digitToInt l))

scanLiteralContent ('\\' : c : cs) loc =
    error (show loc ++ ": unexpected escape: \\" ++ [c])

scanLiteralContent ('\n' : cs) loc =
    case scanLiteralContent cs (incl 1 loc) of
         (string, loc, tokens) -> ('\n' : string, loc, tokens)

scanLiteralContent ('"' : cs) loc =
    ("", incc 1 loc, scan cs (incc 1 loc))

scanLiteralContent (c : cs) loc =
    case scanLiteralContent cs (incc 1 loc) of
         (string, loc, tokens) -> (c : string, loc, tokens)

scanTokInt :: Bool -> String -> Loc -> [Tok]
scanTokInt True input l =
    TokInt ((read head), loc) : scan tail loc
    where (head, tail) = span isDigit input
	  size         = length head
	  loc          = incc size l

scanTokInt False input l =
    TokInt (-(read head), loc) : scan tail loc
    where (head, tail) = span isDigit input
	  size         = length head
	  loc          = incc size l

scanTokString :: String -> Loc -> [Tok]
scanTokString input l =
    scanTokKeyword head loc : scan tail loc
    where (head, tail) = span isIdentNum input
	  size         = length head
	  loc          = incc size l

scanTokKeyword :: String -> Loc -> Tok
scanTokKeyword "binop" loc = TokBinop loc
scanTokKeyword "cjump" loc = TokCJump loc
scanTokKeyword "call" loc  = TokCall loc
scanTokKeyword "const" loc = TokConst loc
scanTokKeyword "end" loc   = TokEnd loc
scanTokKeyword "eseq" loc  = TokESeq loc
scanTokKeyword "frame" loc = TokFrame loc
scanTokKeyword "sxp" loc   = TokExp loc
scanTokKeyword "jump" loc  = TokJump loc
scanTokKeyword "label" loc = TokLabel loc
scanTokKeyword "mem" loc   = TokMem loc
scanTokKeyword "move" loc  = TokMove loc
scanTokKeyword "name" loc  = TokName loc
scanTokKeyword "seq" loc   = TokSeq loc
scanTokKeyword "temp" loc  = TokTemp loc

scanTokKeyword "add" loc     = TokOp (Add, loc)
scanTokKeyword "mul" loc     = TokOp (Mul, loc)
scanTokKeyword "sub" loc     = TokOp (Sub, loc)
scanTokKeyword "div" loc     = TokOp (Div, loc)
scanTokKeyword "lshift" loc  = TokOp (Lshift, loc)
scanTokKeyword "rshift" loc  = TokOp (Rshift, loc)
scanTokKeyword "arshift" loc = TokOp (Arshift, loc)

scanTokKeyword "(+)" loc   = TokOp (Add, loc)
scanTokKeyword "(*)" loc   = TokOp (Mul, loc)
scanTokKeyword "(-)" loc   = TokOp (Sub, loc)
scanTokKeyword "(/)" loc   = TokOp (Div, loc)

scanTokKeyword "eq" loc    = TokRelop (Eq, loc)
scanTokKeyword "ne" loc    = TokRelop (Ne, loc)
scanTokKeyword "lt" loc    = TokRelop (Lt, loc)
scanTokKeyword "gt" loc    = TokRelop (Gt, loc)
scanTokKeyword "le" loc    = TokRelop (Le, loc)
scanTokKeyword "ge" loc    = TokRelop (Ge, loc)
scanTokKeyword "ult" loc   = TokRelop (Ult, loc)
scanTokKeyword "ule" loc   = TokRelop (Ule, loc)
scanTokKeyword "ugt" loc   = TokRelop (Ugt, loc)
scanTokKeyword "uge" loc   = TokRelop (Uge, loc)

scanTokKeyword "(=)"  loc = TokRelop (Eq, loc)
scanTokKeyword "(<>)" loc = TokRelop (Ne, loc)
scanTokKeyword "(<)"  loc = TokRelop (Lt, loc)
scanTokKeyword "(<=)" loc = TokRelop (Le, loc)
scanTokKeyword "(>)"  loc = TokRelop (Gt, loc)
scanTokKeyword "(>=)" loc = TokRelop (Ge, loc)

scanTokKeyword str loc     = TokString (str, loc)
