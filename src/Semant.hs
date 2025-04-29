module Semant (transProg, (:::) (..)) where

import AST hiding (Dec, Exp, Field, FunDec, Ty, Var)
import AST qualified as AST
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.RWS
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Env
import Symbol
import Translate qualified
import Types

type Var = AST.Var Symbol

type Exp = AST.Exp Symbol

type ASTTy = AST.Ty Symbol

type Dec = AST.Dec Symbol

type FunDec = AST.FunDec Symbol

type Field = AST.Field Symbol

data Env = Env
  { envVal :: SymTable EnvEntry,
    envTy :: SymTable Ty,
    envSym :: Map String Symbol
  }
  deriving (Eq, Show, Ord)

instance Semigroup Env where
  Env vEnv1 tEnv1 sEnv1 <> Env vEnv2 tEnv2 sEnv2 =
    Env (vEnv1 <> vEnv2) (tEnv1 <> tEnv2) (sEnv1 <> sEnv2)

instance Monoid Env where
  mempty = Env mempty mempty mempty

initEnv :: Env
initEnv =
  Env
    { envVal = val_tys,
      envTy = ty_tys,
      envSym = val_syms <> ty_syms
    }
  where
    ((val_syms, val_tys), (ty_syms, ty_tys)) = prelude

data Error
  = InvalidType Ty (Set Ty) SourcePos
  | Undefined String (Maybe SourcePos)
  | Error String (Maybe SourcePos)
  deriving (Show, Eq)

newtype TransM a = TransM {runTransM :: RWST Env () Tag (Either Error) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Env,
      MonadWriter (),
      MonadState Tag,
      MonadError Error
    )

instance MonadSymTable TransM EnvEntry where
  askSymTable = asks envVal
  withSymTable f = local $ \env -> env {envVal = f $ envVal env}

instance MonadSymTable TransM Ty where
  askSymTable = asks envTy
  withSymTable f = local $ \env -> env {envTy = f $ envTy env}

data (:::) a b = a ::: b

deannotate :: (a ::: b) -> a
deannotate (a ::: _) = a

typeOf :: (a ::: b) -> b
typeOf (_ ::: b) = b

lookupSym :: String -> TransM (Maybe Symbol)
lookupSym s = (M.!? s) <$> asks envSym

lookupSym' :: String -> Maybe SourcePos -> TransM Symbol
lookupSym' s pos = do
  msym <- lookupSym s
  case msym of
    Nothing -> throwError $ Undefined s pos
    Just sym -> pure sym

lookupVar :: Symbol -> Maybe SourcePos -> TransM Ty
lookupVar sym pos = do
  mentry <- askSym sym
  case mentry of
    Just (VarEntry ty) -> pure ty
    _ -> throwError $ Undefined (symName sym) pos

lookupFun :: Symbol -> Maybe SourcePos -> TransM ([Ty], Ty)
lookupFun sym pos = do
  mentry <- askSym sym
  case mentry of
    Just (FunEntry pts rt) -> pure (pts, rt)
    _ -> throwError $ Undefined (symName sym) pos

lookupTy :: Symbol -> Maybe SourcePos -> TransM Ty
lookupTy sym pos = do
  mty <- askSym sym
  case mty of
    Nothing -> throwError $ Undefined (symName sym) pos
    Just ty -> unpack ty

unpack :: Ty -> TransM Ty
unpack (Name sym) = do
  mty <- askSym sym
  case mty of
    Just (Name sym') -> unpack (Name sym')
    Just t -> pure t
    Nothing -> throwError $ Undefined (symName sym) Nothing
unpack t = pure t

lookupTyNoUnpack :: Symbol -> Maybe SourcePos -> TransM Ty
lookupTyNoUnpack sym pos = do
  mty <- askSym sym
  case mty of
    Nothing -> throwError $ Undefined (symName sym) pos
    Just ty -> pure ty

withSym :: String -> (Symbol -> TransM a) -> TransM a
withSym s m = do
  mname <- (M.!? s) <$> asks envSym
  case mname of
    Just sym -> m sym
    Nothing -> do
      sym <- newSym s
      local (\env -> env {envSym = M.insert s sym $ envSym env}) $ m sym

transProg :: UntypedExp -> Either Error (Exp ::: Ty)
transProg e = fst <$> evalRWST (runTransM $ transExp e) initEnv preludeTag

transVar :: UntypedVar -> TransM (Var ::: Ty)
transVar (SimpleVar s pos) = do
  v <- lookupSym' s $ Just pos
  ty <- lookupVar v $ Just pos
  pure $ SimpleVar v pos ::: ty
transVar (FieldVar v field pos) = do
  v' ::: v_t <- transVar v
  field' <- lookupSym' field $ Just pos
  case fieldType field' v_t of
    Nothing ->
      throwError
        $ Error
          ( "Type "
              <> show v_t
              <> " isn't a record with field "
              <> show field
              <> "."
          )
        $ Just pos
    Just field_t -> do
      field_t' <- unpack field_t
      pure $ FieldVar v' field' pos ::: field_t'
transVar (SubscriptVar v i pos) = do
  v' ::: v_t <- transVar v
  i' ::: i_t <- transExp i
  case elemType v_t of
    Nothing ->
      throwError
        $ Error
          ( "Type " <> show v_t <> " isn't an array."
          )
        $ Just pos
    Just e_t -> do
      eqTypes pos i_t Int
      pure $ SubscriptVar v' i' pos ::: e_t

check_ :: (a ::: Ty) -> [Ty] -> SourcePos -> TransM ()
check_ a tys pos = void $ check a tys pos

check :: (a ::: Ty) -> [Ty] -> SourcePos -> TransM Ty
check a tys pos
  | typeOf a `elem` tys = pure $ typeOf a
  | otherwise = throwError $ InvalidType (typeOf a) (S.fromList tys) pos

transExp :: UntypedExp -> TransM (Exp ::: Ty)
transExp (VarExp v) = do
  v' ::: v_ty <- transVar v
  pure $ VarExp v' ::: v_ty
transExp NilExp = pure $ NilExp ::: Nil
transExp (IntExp i pos) = pure $ IntExp i pos ::: Int
transExp (StringExp s pos) = pure $ StringExp s pos ::: String
transExp (CallExp f args pos) = do
  f' <- lookupSym' f $ Just pos
  (pts, rt) <- lookupFun f' $ Just pos
  args' <- mapM transExp args
  unless (length pts == length args)
    $ throwError
    $ Error
      ( "Function expects "
          <> show (length pts)
          <> " arguments, but "
          <> show (length args)
          <> " were given."
      )
    $ Just pos
  void $ zipWithM (checkTypeAnnot pos) (map typeOf args') pts
  pure $ CallExp f' (map deannotate args') pos ::: rt
transExp (OpExp l op r pos) = do
  lt@(l' ::: l_ty) <- transExp l
  rt@(r' ::: r_ty) <- transExp r
  case op of
    _
      | op `elem` [PlusOp, MinusOp, TimesOp, DivideOp] -> do
          check_ lt [Int] pos
          check_ rt [Int] pos
          pure $ OpExp l' op r' pos ::: Int
      | otherwise -> do
          eqTypes pos l_ty r_ty
          pure $ OpExp l' op r' pos ::: Int
transExp (RecordExp t fields pos) = do
  t_sym <- lookupSym' t $ Just pos
  t <- lookupTy t_sym $ Just pos
  case recordFields t of
    Nothing -> throwError $ InvalidType t S.empty pos
    Just t_fields -> do
      fields' <- forM (zip t_fields fields) $
        \((_, f_ty), (field, e, f_pos)) -> do
          field_sym <- lookupSym' field $ Just f_pos
          e' ::: e_ty <- transExp e
          -- fix
          -- checkTypeAnnot pos e_ty f_ty
          pure (field_sym, e', pos)
      pure $ RecordExp t_sym fields' pos ::: t
transExp (SeqExp es) = do
  es' <- mapM (transExp . fst) es
  let t =
        case es' of
          [] -> Unit
          _ -> typeOf $ last es'
  pure $
    SeqExp
      (zipWith (\(_, pos) (e' ::: _) -> (e', pos)) es es')
      ::: t
transExp (AssignExp v e pos) = do
  v' ::: v_t <- transVar v
  e' ::: e_t <- transExp e
  checkTypeAnnot pos e_t v_t
  pure $ AssignExp v' e' pos ::: Unit
transExp (IfExp c t mf pos) = do
  c' ::: c_t <- transExp c
  eqTypes pos c_t Int
  t' ::: t_t <- transExp t
  mf' <- case mf of
    Nothing -> do
      eqTypes pos t_t Unit
      pure Nothing
    Just f -> do
      f' ::: f_t <- transExp f
      eqTypes pos t_t f_t
      pure $ Just f'
  pure $ IfExp c' t' mf' pos ::: t_t
transExp (WhileExp c b pos) = do
  c' ::: c_t <- transExp c
  eqTypes pos c_t Int
  b' ::: b_t <- transExp b
  eqTypes pos b_t Unit
  pure $ WhileExp c' b' pos ::: b_t
transExp (ForExp i from to b pos) = do
  from' ::: from_t <- transExp from
  eqTypes pos from_t Int
  to' ::: to_t <- transExp to
  eqTypes pos to_t Int
  withSym i $ \i_sym -> do
    b' ::: b_t <- insertSym i_sym (VarEntry Int) $ transExp b
    pure $ ForExp i_sym from' to' b' pos ::: b_t
transExp (BreakExp pos) = pure $ BreakExp pos ::: Unit
transExp (LetExp decs e pos) = do
  (decs', e' ::: t) <- transDecs decs $ \decs' ->
    (decs',) <$> transExp e
  pure $ LetExp decs' e' pos ::: t
transExp (ArrayExp t n e pos) = do
  t_sym <- lookupSym' t $ Just pos
  t' <- lookupTy t_sym $ Just pos
  case elemType t' of
    Nothing ->
      throwError $ InvalidType t' S.empty pos
    Just elem_t -> do
      n' ::: n_t <- transExp n
      eqTypes pos n_t Int
      e' ::: e_t <- transExp e
      eqTypes pos e_t elem_t
      pure $ ArrayExp t_sym n' e' pos ::: t'

transDecs :: [UntypedDec] -> ([Dec] -> TransM a) -> TransM a
transDecs ds m = transDecs' ds []
  where
    transDecs' [] ds' = m $ reverse ds'
    transDecs' (d : ds) ds' =
      transDec d $ \d' ->
        transDecs' ds (d' : ds')

transDec :: UntypedDec -> (Dec -> TransM a) -> TransM a
transDec (FunctionDec decs) m =
  withHeaders decs
  where
    withHeaders [] = m =<< FunctionDec <$> mapM transFunDec decs
    withHeaders (AST.FunDec f params mrt body pos : ds) = do
      params' <- withParams params pure
      body_ty <-
        case mrt of
          Nothing ->
            pure Unit
          Just (rt_s, pos) -> do
            rt_sym <- lookupSym' rt_s $ Just pos
            rt <- lookupTy rt_sym $ Just pos
            pure rt
      withSym f $ \f' ->
        insertSym f' (FunEntry (map typeOf params') body_ty) $
          withHeaders ds

    transFunDec (AST.FunDec f params mrt body pos) = do
      f' <- lookupSym' f $ Just pos
      (pts, rt) <- lookupFun f' $ Just pos
      (params', body' ::: body_ty) <-
        withParams params $ \params' -> (params',) <$> transExp body
      checkTypeAnnot pos body_ty rt
      mrt' <-
        case mrt of
          Nothing -> pure Nothing
          Just (rt_s, pos) -> do
            rt_sym <- lookupSym' rt_s $ Just pos
            pure $ Just (rt_sym, pos)
      pure $ AST.FunDec f' (map deannotate params') mrt' body' pos

    withParams :: [UntypedField] -> ([Field ::: Ty] -> TransM a) -> TransM a
    withParams ps m = withParams' ps []
      where
        withParams' [] fs' = m $ reverse fs'
        withParams' (AST.Field field ty_s pos : fs) fs' =
          withSym field $ \field' -> do
            ty_sym <- lookupSym' ty_s $ Just pos
            ty <- lookupTy ty_sym $ Just pos
            insertSym field' (VarEntry ty) $ withParams' fs (AST.Field field' ty_sym pos ::: ty : fs')
transDec (VarDec s mty e pos) m = do
  e' ::: e_ty <- transExp e
  mtyt <- case mty of
    Nothing -> do
      when (e_ty == Nil) $
        throwError $
          Error "nil expressions must be cosntrained by record types" $
            Just pos
      pure Nothing
    Just (ty_s, ty_pos) -> do
      ty_sym <- lookupSym' ty_s $ Just ty_pos
      ty <- lookupTy ty_sym $ Just ty_pos
      checkTypeAnnot ty_pos e_ty ty
      pure $ Just (ty_sym, ty_pos)
  withSym s $ \sym ->
    insertSym
      sym
      (VarEntry e_ty)
      (m $ VarDec sym mtyt e' pos)
transDec (TypeDec decs) m =
  withHeaders decs
  where
    withHeaders [] = transTypeDecs decs []
    withHeaders ((s, sty, pos) : ds) = do
      withSym s $ \sym ->
        insertSym sym (Name sym) (withHeaders ds)

    transTypeDecs [] ds' = do
      checkTyTable
      m $ TypeDec $ reverse ds'
    transTypeDecs ((s, sty, pos) : ds) ds' = do
      sym <- lookupSym' s $ Just pos
      transTy sty $ \(sty' ::: ty) ->
        insertSym sym ty $
          transTypeDecs ds $
            (sym, sty', pos) : ds'

    checkTyTable :: TransM ()
    checkTyTable =
      void $ traverse checkTy =<< asks envTy
      where
        checkTy (Name sym) = do
          mt <- askSym sym
          case mt of
            Nothing ->
              error $ "checkTy: error, unknown type: " <> show sym
            Just t@(Name {}) ->
              throwError $ Error ("recursive cycle involving " <> show t) Nothing
            _ -> pure ()
        checkTy _ = pure ()

    transTy :: UntypedTy -> (AST.Ty Symbol ::: Ty -> TransM a) -> TransM a
    transTy (NameTy s pos) m = do
      sym <- lookupSym' s $ Just pos
      ty <- lookupTyNoUnpack sym $ Just pos
      m $ NameTy sym pos ::: ty
    transTy (RecordTy fields) m = do
      withFields fields [] $ \fields' -> do
        tag <- newTag
        let ty_fields = map (\(AST.Field sym _ _ ::: ty) -> (sym, ty)) fields'
            ty = Record ty_fields tag
        m $ RecordTy (map deannotate fields') ::: ty
      where
        withFields ::
          [UntypedField] ->
          [Field ::: Ty] ->
          ([Field ::: Ty] -> TransM a) ->
          TransM a
        withFields [] fs' n =
          n $ reverse fs'
        withFields (f : fs) fs' n =
          transField f $ \f' ->
            withFields fs (f' : fs') n

        transField :: UntypedField -> (Field ::: Ty -> TransM a) -> TransM a
        transField (AST.Field field ty_s pos) n =
          withSym field $ \field_sym -> do
            ty_sym <- lookupSym' ty_s $ Just pos
            ty <- lookupTyNoUnpack ty_sym $ Just pos
            n $ AST.Field field_sym ty_sym pos ::: ty
    transTy (ArrayTy s pos) m = do
      sym <- lookupSym' s $ Just pos
      ty <- lookupTyNoUnpack sym $ Just pos
      tag <- newTag
      m $ ArrayTy sym pos ::: Array ty tag

validTypeAnnot :: Ty -> Ty -> Bool
validTypeAnnot Nil (Record _ _) = True
validTypeAnnot e_ty ty = ty == e_ty

checkTypeAnnot :: SourcePos -> Ty -> Ty -> TransM ()
checkTypeAnnot pos e_ty ty =
  unless (e_ty `validTypeAnnot` ty) $
    throwError $
      InvalidType e_ty (S.singleton ty) pos

eqTypes :: SourcePos -> Ty -> Ty -> TransM ()
eqTypes pos t1 t2 =
  unless (okTypes t1 t2) $
    throwError $
      InvalidType t1 (S.singleton t2) pos
  where
    okTypes Record {} Nil = True
    okTypes Nil Record {} = True
    okTypes t1 t2 = t1 == t2
