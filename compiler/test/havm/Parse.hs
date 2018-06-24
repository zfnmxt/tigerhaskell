{-# OPTIONS_GHC -w #-}
module Parse (parse)
where

import Data.Char (ord)

import Ir
import Token
import Location (merge)
import Annotation (Ann (NoAnn, LevelAnn, LocationAnn), annExp, locationAnn)
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn 
	= HappyTerminal (Tok)
	| HappyErrorToken Int
	| HappyAbsSyn4 ([Stm Ann])
	| HappyAbsSyn5 (Exp Ann)
	| HappyAbsSyn6 (Stm Ann)
	| HappyAbsSyn7 ([Exp Ann])

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Tok)
	-> HappyState (Tok) (HappyStk HappyAbsSyn -> [(Tok)] -> m HappyAbsSyn)
	-> [HappyState (Tok) (HappyStk HappyAbsSyn -> [(Tok)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Tok)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Tok)
	-> HappyState (Tok) (HappyStk HappyAbsSyn -> [(Tok)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Tok) (HappyStk HappyAbsSyn -> [(Tok)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Tok)] -> (HappyIdentity) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Tok)
	-> HappyState (Tok) (HappyStk HappyAbsSyn -> [(Tok)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Tok) (HappyStk HappyAbsSyn -> [(Tok)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Tok)] -> (HappyIdentity) HappyAbsSyn)

action_0 (12) = happyShift action_4
action_0 (15) = happyShift action_5
action_0 (18) = happyShift action_6
action_0 (19) = happyShift action_7
action_0 (20) = happyShift action_8
action_0 (23) = happyShift action_9
action_0 (27) = happyShift action_10
action_0 (4) = happyGoto action_11
action_0 (6) = happyGoto action_2
action_0 (8) = happyGoto action_3
action_0 _ = happyReduce_19

action_1 (12) = happyShift action_4
action_1 (15) = happyShift action_5
action_1 (18) = happyShift action_6
action_1 (19) = happyShift action_7
action_1 (20) = happyShift action_8
action_1 (23) = happyShift action_9
action_1 (27) = happyShift action_10
action_1 (6) = happyGoto action_2
action_1 (8) = happyGoto action_3
action_1 _ = happyFail

action_2 (12) = happyShift action_4
action_2 (15) = happyShift action_5
action_2 (18) = happyShift action_6
action_2 (19) = happyShift action_7
action_2 (20) = happyShift action_8
action_2 (23) = happyShift action_9
action_2 (27) = happyShift action_10
action_2 (6) = happyGoto action_2
action_2 (8) = happyGoto action_25
action_2 _ = happyReduce_19

action_3 _ = happyReduce_1

action_4 (26) = happyShift action_24
action_4 _ = happyFail

action_5 (9) = happyShift action_14
action_5 (10) = happyShift action_15
action_5 (13) = happyShift action_16
action_5 (14) = happyShift action_17
action_5 (22) = happyShift action_18
action_5 (24) = happyShift action_19
action_5 (30) = happyShift action_20
action_5 (5) = happyGoto action_23
action_5 _ = happyFail

action_6 (9) = happyShift action_14
action_6 (10) = happyShift action_15
action_6 (13) = happyShift action_16
action_6 (14) = happyShift action_17
action_6 (22) = happyShift action_18
action_6 (24) = happyShift action_19
action_6 (30) = happyShift action_20
action_6 (5) = happyGoto action_22
action_6 _ = happyFail

action_7 (29) = happyShift action_21
action_7 _ = happyFail

action_8 _ = happyReduce_16

action_9 (9) = happyShift action_14
action_9 (10) = happyShift action_15
action_9 (13) = happyShift action_16
action_9 (14) = happyShift action_17
action_9 (22) = happyShift action_18
action_9 (24) = happyShift action_19
action_9 (30) = happyShift action_20
action_9 (5) = happyGoto action_13
action_9 _ = happyFail

action_10 (12) = happyShift action_4
action_10 (15) = happyShift action_5
action_10 (18) = happyShift action_6
action_10 (19) = happyShift action_7
action_10 (20) = happyShift action_8
action_10 (23) = happyShift action_9
action_10 (27) = happyShift action_10
action_10 (6) = happyGoto action_2
action_10 (8) = happyGoto action_12
action_10 _ = happyReduce_19

action_11 (31) = happyAccept
action_11 _ = happyFail

action_12 (28) = happyShift action_36
action_12 _ = happyFail

action_13 (9) = happyShift action_14
action_13 (10) = happyShift action_15
action_13 (13) = happyShift action_16
action_13 (14) = happyShift action_17
action_13 (22) = happyShift action_18
action_13 (24) = happyShift action_19
action_13 (30) = happyShift action_20
action_13 (5) = happyGoto action_35
action_13 _ = happyFail

action_14 (25) = happyShift action_34
action_14 _ = happyFail

action_15 (9) = happyShift action_14
action_15 (10) = happyShift action_15
action_15 (13) = happyShift action_16
action_15 (14) = happyShift action_17
action_15 (22) = happyShift action_18
action_15 (24) = happyShift action_19
action_15 (30) = happyShift action_20
action_15 (5) = happyGoto action_33
action_15 _ = happyFail

action_16 (17) = happyShift action_32
action_16 _ = happyFail

action_17 (12) = happyShift action_4
action_17 (15) = happyShift action_5
action_17 (18) = happyShift action_6
action_17 (19) = happyShift action_7
action_17 (20) = happyShift action_8
action_17 (23) = happyShift action_9
action_17 (27) = happyShift action_10
action_17 (6) = happyGoto action_31
action_17 _ = happyFail

action_18 (9) = happyShift action_14
action_18 (10) = happyShift action_15
action_18 (13) = happyShift action_16
action_18 (14) = happyShift action_17
action_18 (22) = happyShift action_18
action_18 (24) = happyShift action_19
action_18 (30) = happyShift action_20
action_18 (5) = happyGoto action_30
action_18 _ = happyFail

action_19 (29) = happyShift action_29
action_19 _ = happyFail

action_20 (29) = happyShift action_28
action_20 _ = happyFail

action_21 (21) = happyShift action_27
action_21 _ = happyReduce_14

action_22 _ = happyReduce_11

action_23 _ = happyReduce_10

action_24 (9) = happyShift action_14
action_24 (10) = happyShift action_15
action_24 (13) = happyShift action_16
action_24 (14) = happyShift action_17
action_24 (22) = happyShift action_18
action_24 (24) = happyShift action_19
action_24 (30) = happyShift action_20
action_24 (5) = happyGoto action_26
action_24 _ = happyFail

action_25 _ = happyReduce_20

action_26 (9) = happyShift action_14
action_26 (10) = happyShift action_15
action_26 (13) = happyShift action_16
action_26 (14) = happyShift action_17
action_26 (22) = happyShift action_18
action_26 (24) = happyShift action_19
action_26 (30) = happyShift action_20
action_26 (5) = happyGoto action_41
action_26 _ = happyFail

action_27 _ = happyReduce_15

action_28 _ = happyReduce_4

action_29 _ = happyReduce_3

action_30 _ = happyReduce_6

action_31 (9) = happyShift action_14
action_31 (10) = happyShift action_15
action_31 (13) = happyShift action_16
action_31 (14) = happyShift action_17
action_31 (22) = happyShift action_18
action_31 (24) = happyShift action_19
action_31 (30) = happyShift action_20
action_31 (5) = happyGoto action_40
action_31 _ = happyFail

action_32 _ = happyReduce_2

action_33 (9) = happyShift action_14
action_33 (10) = happyShift action_15
action_33 (13) = happyShift action_16
action_33 (14) = happyShift action_17
action_33 (22) = happyShift action_18
action_33 (24) = happyShift action_19
action_33 (30) = happyShift action_20
action_33 (5) = happyGoto action_38
action_33 (7) = happyGoto action_39
action_33 _ = happyReduce_17

action_34 (9) = happyShift action_14
action_34 (10) = happyShift action_15
action_34 (13) = happyShift action_16
action_34 (14) = happyShift action_17
action_34 (22) = happyShift action_18
action_34 (24) = happyShift action_19
action_34 (30) = happyShift action_20
action_34 (5) = happyGoto action_37
action_34 _ = happyFail

action_35 _ = happyReduce_9

action_36 _ = happyReduce_13

action_37 (9) = happyShift action_14
action_37 (10) = happyShift action_15
action_37 (13) = happyShift action_16
action_37 (14) = happyShift action_17
action_37 (22) = happyShift action_18
action_37 (24) = happyShift action_19
action_37 (30) = happyShift action_20
action_37 (5) = happyGoto action_45
action_37 _ = happyFail

action_38 (9) = happyShift action_14
action_38 (10) = happyShift action_15
action_38 (13) = happyShift action_16
action_38 (14) = happyShift action_17
action_38 (22) = happyShift action_18
action_38 (24) = happyShift action_19
action_38 (30) = happyShift action_20
action_38 (5) = happyGoto action_38
action_38 (7) = happyGoto action_44
action_38 _ = happyReduce_17

action_39 (11) = happyShift action_43
action_39 _ = happyFail

action_40 _ = happyReduce_8

action_41 (9) = happyShift action_14
action_41 (10) = happyShift action_15
action_41 (13) = happyShift action_16
action_41 (14) = happyShift action_17
action_41 (22) = happyShift action_18
action_41 (24) = happyShift action_19
action_41 (30) = happyShift action_20
action_41 (5) = happyGoto action_42
action_41 _ = happyFail

action_42 (9) = happyShift action_14
action_42 (10) = happyShift action_15
action_42 (13) = happyShift action_16
action_42 (14) = happyShift action_17
action_42 (22) = happyShift action_18
action_42 (24) = happyShift action_19
action_42 (30) = happyShift action_20
action_42 (5) = happyGoto action_46
action_42 _ = happyFail

action_43 _ = happyReduce_7

action_44 _ = happyReduce_18

action_45 _ = happyReduce_5

action_46 _ = happyReduce_12

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_2  5 happyReduction_2
happyReduction_2 (HappyTerminal (TokInt happy_var_2))
	(HappyTerminal (TokConst happy_var_1))
	 =  HappyAbsSyn5
		 (Const (LocationAnn $ merge happy_var_1 (snd happy_var_2)) (fst happy_var_2)
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 (HappyTerminal (TokString happy_var_2))
	(HappyTerminal (TokName happy_var_1))
	 =  HappyAbsSyn5
		 (Name (LocationAnn $ merge happy_var_1 (snd happy_var_2)) (fst happy_var_2)
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_2  5 happyReduction_4
happyReduction_4 (HappyTerminal (TokString happy_var_2))
	(HappyTerminal (TokTemp happy_var_1))
	 =  HappyAbsSyn5
		 (Temp (LocationAnn $ merge happy_var_1 (snd happy_var_2)) (fst happy_var_2)
	)
happyReduction_4 _ _  = notHappyAtAll 

happyReduce_5 = happyReduce 4 5 happyReduction_5
happyReduction_5 ((HappyAbsSyn5  happy_var_4) `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	(HappyTerminal (TokOp happy_var_2)) `HappyStk`
	(HappyTerminal (TokBinop happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Binop (LocationAnn $ merge happy_var_1 (locationAnn $ annExp happy_var_4)) (fst happy_var_2) happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_6 = happySpecReduce_2  5 happyReduction_6
happyReduction_6 (HappyAbsSyn5  happy_var_2)
	(HappyTerminal (TokMem happy_var_1))
	 =  HappyAbsSyn5
		 (Mem (LocationAnn $ merge happy_var_1 (locationAnn $ annExp happy_var_2)) happy_var_2
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happyReduce 4 5 happyReduction_7
happyReduction_7 ((HappyTerminal (TokCallEnd happy_var_4)) `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	(HappyTerminal (TokCall happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Call (LocationAnn $ merge happy_var_1 happy_var_4) happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_3  5 happyReduction_8
happyReduction_8 (HappyAbsSyn5  happy_var_3)
	(HappyAbsSyn6  happy_var_2)
	(HappyTerminal (TokESeq happy_var_1))
	 =  HappyAbsSyn5
		 (ESeq (LocationAnn $ merge happy_var_1 (locationAnn $ annExp happy_var_3)) happy_var_2 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  6 happyReduction_9
happyReduction_9 (HappyAbsSyn5  happy_var_3)
	(HappyAbsSyn5  happy_var_2)
	(HappyTerminal (TokMove happy_var_1))
	 =  HappyAbsSyn6
		 (Move (LocationAnn $ merge happy_var_1 (locationAnn $ annExp happy_var_3)) happy_var_2 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_2  6 happyReduction_10
happyReduction_10 (HappyAbsSyn5  happy_var_2)
	(HappyTerminal (TokExp happy_var_1))
	 =  HappyAbsSyn6
		 (Sxp (LocationAnn $ merge happy_var_1 (locationAnn $ annExp happy_var_2)) happy_var_2
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_2  6 happyReduction_11
happyReduction_11 (HappyAbsSyn5  happy_var_2)
	(HappyTerminal (TokJump happy_var_1))
	 =  HappyAbsSyn6
		 (Jump (LocationAnn $ merge happy_var_1 (locationAnn $ annExp happy_var_2)) happy_var_2
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happyReduce 6 6 happyReduction_12
happyReduction_12 ((HappyAbsSyn5  happy_var_6) `HappyStk`
	(HappyAbsSyn5  happy_var_5) `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	(HappyTerminal (TokRelop happy_var_2)) `HappyStk`
	(HappyTerminal (TokCJump happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (CJump (LocationAnn $ merge happy_var_1 (locationAnn $ annExp happy_var_6)) (fst happy_var_2) happy_var_3 happy_var_4 happy_var_5 happy_var_6
	) `HappyStk` happyRest

happyReduce_13 = happySpecReduce_3  6 happyReduction_13
happyReduction_13 (HappyTerminal (TokSeqEnd happy_var_3))
	(HappyAbsSyn4  happy_var_2)
	(HappyTerminal (TokSeq happy_var_1))
	 =  HappyAbsSyn6
		 (Seq (LocationAnn $ merge happy_var_1 happy_var_3) happy_var_2
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_2  6 happyReduction_14
happyReduction_14 (HappyTerminal (TokString happy_var_2))
	(HappyTerminal (TokLabel happy_var_1))
	 =  HappyAbsSyn6
		 (Label (LocationAnn $ merge happy_var_1 (snd happy_var_2)) (fst happy_var_2)
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  6 happyReduction_15
happyReduction_15 (HappyTerminal (TokLiteral happy_var_3))
	(HappyTerminal (TokString happy_var_2))
	(HappyTerminal (TokLabel happy_var_1))
	 =  HappyAbsSyn6
		 (Literal (LocationAnn $ merge happy_var_1 (snd happy_var_3)) (fst happy_var_2) $ map ord (fst happy_var_3)
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  6 happyReduction_16
happyReduction_16 (HappyTerminal (TokLabelEnd happy_var_1))
	 =  HappyAbsSyn6
		 (LabelEnd $ LocationAnn happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_0  7 happyReduction_17
happyReduction_17  =  HappyAbsSyn7
		 ([]
	)

happyReduce_18 = happySpecReduce_2  7 happyReduction_18
happyReduction_18 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 : happy_var_2
	)
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_0  8 happyReduction_19
happyReduction_19  =  HappyAbsSyn4
		 ([]
	)

happyReduce_20 = happySpecReduce_2  8 happyReduction_20
happyReduction_20 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1 : happy_var_2
	)
happyReduction_20 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 31 31 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokBinop happy_dollar_dollar -> cont 9;
	TokCall happy_dollar_dollar -> cont 10;
	TokCallEnd happy_dollar_dollar -> cont 11;
	TokCJump happy_dollar_dollar -> cont 12;
	TokConst happy_dollar_dollar -> cont 13;
	TokESeq happy_dollar_dollar -> cont 14;
	TokExp happy_dollar_dollar -> cont 15;
	TokFrame happy_dollar_dollar -> cont 16;
	TokInt happy_dollar_dollar -> cont 17;
	TokJump happy_dollar_dollar -> cont 18;
	TokLabel happy_dollar_dollar -> cont 19;
	TokLabelEnd happy_dollar_dollar -> cont 20;
	TokLiteral happy_dollar_dollar -> cont 21;
	TokMem happy_dollar_dollar -> cont 22;
	TokMove happy_dollar_dollar -> cont 23;
	TokName happy_dollar_dollar -> cont 24;
	TokOp happy_dollar_dollar -> cont 25;
	TokRelop happy_dollar_dollar -> cont 26;
	TokSeq happy_dollar_dollar -> cont 27;
	TokSeqEnd happy_dollar_dollar -> cont 28;
	TokString happy_dollar_dollar -> cont 29;
	TokTemp happy_dollar_dollar -> cont 30;
	_ -> happyError' (tk:tks)
	}

happyError_ 31 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = return
    (<*>) = ap
instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Tok)] -> HappyIdentity a
happyError' = HappyIdentity . happyError

parse tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


happyError :: [Tok] -> a
happyError (tok : toks) = error $ "Parse error before " ++ (show tok)
happyError []           = error $ "Parse error at end of file."
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4










































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}

















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}

{-# LINE 46 "templates/GenericTemplate.hs" #-}








{-# LINE 67 "templates/GenericTemplate.hs" #-}

{-# LINE 77 "templates/GenericTemplate.hs" #-}

{-# LINE 86 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 256 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 322 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
