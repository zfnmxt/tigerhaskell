{

module Parse (parse)
where

import Data.Char (ord)

import Ir
import Token
import Location (merge)
import Annotation (Ann (NoAnn, LevelAnn, LocationAnn), annExp, locationAnn)

}

%name parse
%tokentype { Tok }

%token
  "BINOP"      { TokBinop $$ }
  "CALL"       { TokCall $$ }
  "CALL END."  { TokCallEnd $$ }
  "CJUMP"      { TokCJump $$ }
  "CONST"      { TokConst $$ }
  "ESEQ"       { TokESeq $$ }
  "EXP"        { TokExp $$ }
  "FRAME"      { TokFrame $$ }
  int          { TokInt $$ }
  "JUMP"       { TokJump $$ }
  "LABEL"      { TokLabel $$ }
  "LABEL END." { TokLabelEnd $$ }
  literal      { TokLiteral $$ }
  "MEM"        { TokMem $$ }
  "MOVE"       { TokMove $$ }
  "NAME"       { TokName $$ }
  op           { TokOp $$ }
  rop          { TokRelop $$ }
  "SEQ"        { TokSeq $$ }
  "SEQ END."   { TokSeqEnd $$ }
  string       { TokString $$ }
  "TEMP"       { TokTemp $$ }

%%

Program :: { [Stm Ann] }
Program : Stms { $1 }

Exp :: { Exp Ann }
Exp : "CONST" int                 { Const (LocationAnn $ merge $1 (snd $2)) (fst $2) }
    | "NAME" string               { Name (LocationAnn $ merge $1 (snd $2)) (fst $2) }
    | "TEMP" string               { Temp (LocationAnn $ merge $1 (snd $2)) (fst $2) }
    | "BINOP" op Exp Exp          { Binop (LocationAnn $ merge $1 (locationAnn $ annExp $4)) (fst $2) $3 $4 }
    | "MEM" Exp                   { Mem (LocationAnn $ merge $1 (locationAnn $ annExp $2)) $2 }
    | "CALL" Exp Exps "CALL END." { Call (LocationAnn $ merge $1 $4) $2 $3 }
    | "ESEQ" Stm Exp              { ESeq (LocationAnn $ merge $1 (locationAnn $ annExp $3)) $2 $3 }

Stm :: { Stm Ann }
Stm : "MOVE" Exp Exp                    { Move (LocationAnn $ merge $1 (locationAnn $ annExp $3)) $2 $3 }
    | "EXP" Exp                         { Sxp (LocationAnn $ merge $1 (locationAnn $ annExp $2)) $2 }
    | "JUMP" Exp                        { Jump (LocationAnn $ merge $1 (locationAnn $ annExp $2)) $2 }
    | "CJUMP" rop Exp Exp Exp Exp       { CJump (LocationAnn $ merge $1 (locationAnn $ annExp $6)) (fst $2) $3 $4 $5 $6 }
    | "SEQ" Stms "SEQ END."             { Seq (LocationAnn $ merge $1 $3) $2}
    | "LABEL" string                    { Label (LocationAnn $ merge $1 (snd $2)) (fst $2) }
    | "LABEL" string literal            { Literal (LocationAnn $ merge $1 (snd $3)) (fst $2) $ map ord (fst $3) }
    | "LABEL END."                      { LabelEnd $ LocationAnn $1 }

Exps :: { [Exp Ann] }
Exps : {- empty -} { [] }
     | Exp Exps    { $1 : $2 }

Stms :: { [Stm Ann] }
Stms : {- empty -} { [] }
     | Stm Stms    { $1 : $2 }

{

happyError :: [Tok] -> a
happyError (tok : toks) = error $ "Parse error before " ++ (show tok)
happyError []           = error $ "Parse error at end of file."

}
