{-# LANGUAGE RecordWildCards #-}

module STEnv where

import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import qualified Control.Monad.Trans.State.Lazy as S
import Control.Monad.State.Lazy (lift)
import Control.Monad.Trans.State.Lazy (StateT)

import AST
import Types
import Temp
import Frame
import qualified Frame as F

type TError  = String
type STEnvT  = StateT Env (Either TError)

genError :: Show a => a -> String -> STEnvT b
genError x s = lift . Left $ "Error on term: " ++ show x ++ " with error msg: " ++ s

--------------------------------------------------------------------------------
-- Level and Access
--------------------------------------------------------------------------------
data Level = Outermost | Level { _levelParent  :: Level
                               , _levelFrame   :: Frame
                               } deriving (Eq, Show)

data VAccess = VAccess { _accessLevel   :: Level
                       , _accessLoc     :: FAccess
                       }
              deriving (Eq, Show)

newLevel :: Level -> Label -> [Bool] -> Level
newLevel parent name args = Level parent frame
  where frame = F.newFrame name (True:args)

allocLocal :: Level -> Temp -> Bool -> (Level, VAccess)
allocLocal Outermost _ _ = error "oops"
allocLocal level@Level{..} temp esc = (level', VAccess level' fAccess')
  where level' = level { _levelFrame = frame'}
        (frame', fAccess')
         | esc       = F.allocMem _levelFrame
         | otherwise = F.allocReg _levelFrame temp

setLevel :: Level -> STEnvT ()
setLevel level = do
  env@Env{..} <- S.get
  S.put env {_envLevel = level}

resParent :: VEnvEntry -> STEnvT ()
resParent FunEntry{..} = do
  case _funEntryLevel of
    Outermost   -> error "oops"
    Level{..} -> setLevel _levelParent

intLevel :: Level -> Int
intLevel Outermost = 0
intLevel Level{..} = intLevel _levelParent + 1

--------------------------------------------------------------------------------
-- Env Maps
--------------------------------------------------------------------------------
data VEnvEntry = VarEntry {_varEntryTy :: Ty, _varEntryAccess  :: VAccess}
               | FunEntry { _funEntryArgTys   :: [Ty]
                          , _funEntryRetTy    :: Ty
                          , _funEntryLevel    :: Level
                          , _funEntryLabel    :: Label
                          , _funEntryLevelNum :: Int
                          }
               deriving (Show, Eq)

type EnvV = Map Id VEnvEntry
type EnvT = Map TypeId Ty
data Env  = Env { _envV      :: EnvV
                , _envT      :: EnvT
                , _envTemp   :: Int
                , _envLabel  :: Int
                , _envLevel  :: Level
                , _envLevels :: [Level]
                } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Initial env
--------------------------------------------------------------------------------
_PREDEFINED_FUNCS = 10

baseTEnv = M.fromList [("int", Int), ("string", String)]
baseVEnv = M.fromList [ ("print",     FunEntry [String] Unit Outermost (Label 0)             0 )
                      , ("flush",     FunEntry [] Unit Outermost (Label 1)                   0 )
                      , ("getchar",   FunEntry [] String Outermost (Label 2)                 0 )
                      , ("ord",       FunEntry [String] Int Outermost (Label 3)              0 )
                      , ("chr",       FunEntry [Int] String Outermost (Label 4)              0 )
                      , ("size",      FunEntry [String] Int Outermost (Label 5)              0 )
                      , ("substring", FunEntry [String, Int, Int] String Outermost (Label 6) 0 )
                      , ("concat",    FunEntry [String, String] String Outermost (Label 7)   0 )
                      , ("not",       FunEntry [Int] Int Outermost (Label 8)                 0 )
                      , ("exit",      FunEntry [Int] Unit Outermost (Label 9)                0 )
                      ]

initEnv = Env { _envV       = baseVEnv
              , _envT       = baseTEnv
              , _envTemp    = 1
              , _envLabel   = _PREDEFINED_FUNCS
              , _envLevels  = [Level Outermost (newFrame (Label 0) [])]
              , _envLevel   = Level Outermost (newFrame (Label 0) [])
              }

--------------------------------------------------------------------------------
-- Env functions
--------------------------------------------------------------------------------

insertVar :: Bool -> Id -> Ty -> STEnvT ()
insertVar esc id ty = do
  env@Env{..} <- S.get
  temp        <- mkTemp
  let (level', access) = allocLocal _envLevel temp esc
  S.put $ env { _envV     = M.insert id (VarEntry ty access) _envV
              , _envLevel = level'
              }

insertFun :: [Bool] -> Id -> [Ty] -> Ty -> STEnvT ()
insertFun escs id argTys resTy = do
  env@Env{..} <- S.get
  levelLabel  <- mkLabel
  funLabel    <- mkLabel
  level       <- getLevel
  let level  = newLevel _envLevel levelLabel escs
  S.put $ env { _envV      = M.insert id (FunEntry argTys resTy level funLabel (intLevel level)) _envV
              , _envLevel  = level
              , _envLevels = level:_envLevels
              }

lookupVar :: Id -> STEnvT VEnvEntry
lookupVar id = do
  Env{..} <- S.get
  case M.lookup id _envV of
    Just var@VarEntry{..} -> return var
    Just FunEntry{..}     -> genError id "found function instead of var"
    Nothing               -> genError id "var not found"

lookupFun :: Id -> STEnvT VEnvEntry
lookupFun id = do
  Env{..} <- S.get
  case M.lookup id _envV of
    Just VarEntry{..}     -> genError id "found var instead of function"
    Just fun@FunEntry{..} -> return fun
    Nothing               -> genError id "var not found"

insertTy :: TypeId -> Ty -> STEnvT ()
insertTy typeId ty = do
  env@Env{..} <- S.get
  S.put $ env { _envT =  M.insert typeId ty _envT }

lookupTy :: TypeId -> STEnvT Ty
lookupTy tId = do
  Env{..} <- S.get
  case M.lookup tId _envT of
    Just t  -> return t
    Nothing -> genError tId "type not found"

mkTemp :: STEnvT Temp
mkTemp = do
   env@Env{..} <- S.get
   S.put  $ env { _envTemp = _envTemp + 1}
   return $ Temp _envTemp

mkLabel :: STEnvT Label
mkLabel = do
   env@Env{..} <- S.get
   S.put  $ env { _envLabel = _envLabel + 1}
   return $ Label _envLabel

getLevel :: STEnvT Level
getLevel = do
  Env{..} <- S.get
  return $ _envLevel

