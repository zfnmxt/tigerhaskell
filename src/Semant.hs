{-# LANGUAGE UndecidableInstances #-}

module Semant (transProg) where

import AST hiding (Dec, Exp, Field, FunDec, Ty, Var)
import AST qualified as AST
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.RWS
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Env
import Frame (Frame)
import Symbol
import Temp qualified
import Translate
import TypeCheck
import Types

-- data Error
--  = Error String (Maybe SourcePos)
--  deriving (Show, Eq)
--
-- data TransEntry frame
--  = VarEntry (Access frame) Ty
--  | FunEntry (Level frame) Temp.Label [Ty] Ty
--
-- data Env frame = Env
--  { envVal :: SymTable (TransEntry frame),
--    envLevel :: Level frame
--  }

-- newtype TransM frame a = TransM {runTransM :: ExceptT Error (RWS (Env frame) () Tag) a}
--  deriving
--    ( Functor,
--      Applicative,
--      Monad,
--      MonadReader (Env frame),
--      MonadWriter (),
--      MonadState Tag,
--      MonadError Error
--    )

-- unewtype TransM frame a = TransM {runTransM :: ExceptT Error (RWS (Env frame) () Tag) a}
--
transProg = undefined

--
-- transExp :: (Frame frame) => Exp -> TransM frame a
-- transExp = undefined
--
-- transDec :: (Frame frame) => Dec -> TransM frame a -> TransM frame a
-- transDec (FunctionDec decs) m = transFunDec decs
--  where
--    transFunDec (AST.FunDec f params mrt body pos : ds) = do
--      lvl <- asks envLevel
--      let lvl' = newLevel lvl (Temp.Label f) (map (const True) params)
--      insertSym f (FunEntry lvl') (Temp.Label f)
--      local (\env -> env {

type Var = AST.Var Symbol

type Exp = AST.Exp Symbol

type Dec = AST.Dec Symbol

type Field = AST.Field Symbol

data Env frame = Env
  { envVal :: SymTable (EnvEntry frame),
    envTy :: SymTable Ty,
    envSym :: Map String Symbol
  }

deriving instance (Show (EnvEntry frame)) => Show (Env frame)

deriving instance (Eq (EnvEntry frame)) => Eq (Env frame)

deriving instance (Ord (EnvEntry frame)) => Ord (Env frame)

instance Semigroup (Env frame) where
  Env vEnv1 tEnv1 sEnv1 <> Env vEnv2 tEnv2 sEnv2 =
    Env (vEnv1 <> vEnv2) (tEnv1 <> tEnv2) (sEnv1 <> sEnv2)

instance Monoid (Env frame) where
  mempty = Env mempty mempty mempty

initEnv :: (Frame frame) => Env frame
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

newtype TypeCheckM frame a = TypeCheckM {runTypeCheckM :: ExceptT Error (RWS (Env frame) () Tag) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader (Env frame),
      MonadWriter (),
      MonadState Tag,
      MonadError Error
    )

instance (Frame frame) => MonadSymTable (TypeCheckM frame) (EnvEntry frame) where
  askSymTable = asks envVal
  withSymTable f = local $ \env -> env {envVal = f $ envVal env}

instance (Frame frame) => MonadSymTable (TypeCheckM frame) Ty where
  askSymTable = asks envTy
  withSymTable f = local $ \env -> env {envTy = f $ envTy env}

data (:::) a b = a ::: b

deannotate :: (a ::: b) -> a
deannotate (a ::: _) = a

typeOf :: (a ::: b) -> b
typeOf (_ ::: b) = b

lookupSym :: (Frame frame) => String -> TypeCheckM frame (Maybe Symbol)
lookupSym s = (M.!? s) <$> asks envSym

lookupSym' :: (Frame frame) => String -> Maybe SourcePos -> TypeCheckM frame Symbol
lookupSym' s pos = do
  msym <- lookupSym s
  case msym of
    Nothing -> throwError $ Undefined s pos
    Just sym -> pure sym

lookupVar :: (Frame frame) => Symbol -> Maybe SourcePos -> TypeCheckM frame Ty
lookupVar sym pos = do
  mentry <- askSym sym
  case mentry of
    Just (VarEntry _ ty) -> pure ty
    _ -> throwError $ Undefined (symName sym) pos

lookupFun :: (Frame frame) => Symbol -> Maybe SourcePos -> TypeCheckM frame ([Ty], Ty)
lookupFun sym pos = do
  mentry <- askSym sym
  case mentry of
    Just (FunEntry _ _ pts rt) -> pure (pts, rt)
    _ -> throwError $ Undefined (symName sym) pos

lookupTy :: (Frame frame) => Symbol -> Maybe SourcePos -> TypeCheckM frame Ty
lookupTy sym pos = do
  mty <-
    runMaybeT $
      MaybeT . unpack =<< MaybeT (askSym sym)
  case mty of
    Nothing -> throwError $ Undefined (symName sym) Nothing
    Just ty -> pure ty

lookupTyNoUnpack :: (Frame frame) => Symbol -> Maybe SourcePos -> TypeCheckM frame Ty
lookupTyNoUnpack sym pos = do
  mty <- askSym sym
  case mty of
    Nothing -> throwError $ Undefined (symName sym) pos
    Just ty -> pure ty

withSym :: (Frame frame) => String -> (Symbol -> TypeCheckM frame a) -> TypeCheckM frame a
withSym s m = do
  mname <- (M.!? s) <$> asks envSym
  case mname of
    Just sym -> m sym
    Nothing -> do
      sym <- newSym s
      local (\env -> env {envSym = M.insert s sym $ envSym env}) $ m sym

checkProg :: UntypedExp -> Either Error (Exp ::: Ty)
checkProg e = fst $ (evalRWS $ runExceptT $ runTypeCheckM $ checkExp e) initEnv preludeTag

checkVar :: (Frame frame) => UntypedVar -> TypeCheckM frame (Var ::: Ty)
checkVar (SimpleVar s pos) = do
  v <- lookupSym' s $ Just pos
  ty <- lookupVar v $ Just pos
  pure $ SimpleVar v pos ::: ty
checkVar (FieldVar v field pos) = do
  v' ::: v_t <- checkVar v
  field' <- lookupSym' field $ Just pos
  mfield_t <- fieldType field' v_t
  case mfield_t of
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
      pure $ FieldVar v' field' pos ::: field_t
checkVar (SubscriptVar v i pos) = do
  v' ::: v_t <- checkVar v
  i' ::: i_t <- checkExp i
  case elemType v_t of
    Nothing ->
      throwError $
        Error ("Type " <> show v_t <> " isn't an array.") $
          Just pos
    Just e_t -> do
      compatTypes pos i_t Int
      pure $ SubscriptVar v' i' pos ::: e_t

checkExp :: (Frame frame) => UntypedExp -> TypeCheckM frame (Exp ::: Ty)
checkExp (VarExp v) = do
  v' ::: v_ty <- checkVar v
  pure $ VarExp v' ::: v_ty
checkExp NilExp = pure $ NilExp ::: Nil
checkExp (IntExp i pos) = pure $ IntExp i pos ::: Int
checkExp (StringExp s pos) = pure $ StringExp s pos ::: String
checkExp (CallExp f args pos) = do
  f' <- lookupSym' f $ Just pos
  (pts, rt) <- lookupFun f' $ Just pos
  args' <- mapM checkExp args
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
  void $ zipWithM (compatTypes pos) (map typeOf args') pts
  pure $ CallExp f' (map deannotate args') pos ::: rt
checkExp (OpExp l op r pos) = do
  lt@(l' ::: l_ty) <- checkExp l
  rt@(r' ::: r_ty) <- checkExp r
  case op of
    _
      | op `elem` [PlusOp, MinusOp, TimesOp, DivideOp] -> do
          check lt [Int] pos
          check rt [Int] pos
          pure $ OpExp l' op r' pos ::: Int
      | otherwise -> do
          compatTypes pos l_ty r_ty
          pure $ OpExp l' op r' pos ::: Int
  where
    check :: (a ::: Ty) -> [Ty] -> SourcePos -> TypeCheckM frame Ty
    check a tys pos
      | typeOf a `elem` tys = pure $ typeOf a
      | otherwise = throwError $ InvalidType (typeOf a) (S.fromList tys) pos
checkExp (RecordExp t fields pos) = do
  t_sym <- lookupSym' t $ Just pos
  t <- lookupTy t_sym $ Just pos
  case recordFields t of
    Nothing -> throwError $ InvalidType t S.empty pos
    Just t_fields -> do
      fields' <- forM (zip t_fields fields) $
        \((_, f_ty), (field, e, f_pos)) -> do
          field_sym <- lookupSym' field $ Just f_pos
          mf_ty' <- unpack f_ty
          case mf_ty' of
            Nothing -> throwError $ Undefined (show f_ty) $ Just f_pos
            Just f_ty' -> do
              e' ::: e_ty <- checkExp e
              compatTypes pos e_ty f_ty'
              pure (field_sym, e', pos)
      pure $ RecordExp t_sym fields' pos ::: t
checkExp (SeqExp es) = do
  es' <- mapM (checkExp . fst) es
  let t =
        case es' of
          [] -> Unit
          _ -> typeOf $ last es'
  pure $
    SeqExp
      (zipWith (\(_, pos) (e' ::: _) -> (e', pos)) es es')
      ::: t
checkExp (AssignExp v e pos) = do
  v' ::: v_t <- checkVar v
  e' ::: e_t <- checkExp e
  compatTypes pos e_t v_t
  pure $ AssignExp v' e' pos ::: Unit
checkExp (IfExp c t mf pos) = do
  c' ::: c_t <- checkExp c
  compatTypes pos c_t Int
  t' ::: t_t <- checkExp t
  mf' <- case mf of
    Nothing -> do
      compatTypes pos t_t Unit
      pure Nothing
    Just f -> do
      f' ::: f_t <- checkExp f
      compatTypes pos t_t f_t
      pure $ Just f'
  pure $ IfExp c' t' mf' pos ::: t_t
checkExp (WhileExp c b pos) = do
  c' ::: c_t <- checkExp c
  compatTypes pos c_t Int
  b' ::: b_t <- checkExp b
  compatTypes pos b_t Unit
  pure $ WhileExp c' b' pos ::: b_t
checkExp (ForExp i from to b pos) = do
  from' ::: from_t <- checkExp from
  compatTypes pos from_t Int
  to' ::: to_t <- checkExp to
  compatTypes pos to_t Int
  withSym i $ \i_sym -> do
    b' ::: b_t <- insertSym i_sym (VarEntry undefined Int) $ checkExp b -- fix
    pure $ ForExp i_sym from' to' b' pos ::: b_t
checkExp (BreakExp pos) = pure $ BreakExp pos ::: Unit
checkExp (LetExp decs e pos) = do
  (decs', e' ::: t) <- checkDecs decs $ \decs' ->
    (decs',) <$> checkExp e
  pure $ LetExp decs' e' pos ::: t
checkExp (ArrayExp t n e pos) = do
  t_sym <- lookupSym' t $ Just pos
  t' <- lookupTy t_sym $ Just pos
  case elemType t' of
    Nothing ->
      throwError $ InvalidType t' S.empty pos
    Just elem_t -> do
      n' ::: n_t <- checkExp n
      compatTypes pos n_t Int
      e' ::: e_t <- checkExp e
      compatTypes pos e_t elem_t
      pure $ ArrayExp t_sym n' e' pos ::: t'

checkDecs :: [UntypedDec] -> ([Dec] -> TypeCheckM frame a) -> TypeCheckM frame a
checkDecs ds m = checkDecs' ds []
  where
    checkDecs' [] ds' = m $ reverse ds'
    checkDecs' (d : ds) ds' =
      checkDec d $ \d' ->
        checkDecs' ds (d' : ds')

checkDec :: (Frame frame) => UntypedDec -> (Dec -> TypeCheckM frame a) -> TypeCheckM frame a
checkDec (FunctionDec decs) m =
  withHeaders decs
  where
    withHeaders [] = m =<< FunctionDec <$> mapM checkFunDec decs
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
        insertSym f' (FunEntry undefined undefined (map typeOf params') body_ty) $ -- fix
          withHeaders ds

    checkFunDec (AST.FunDec f params mrt body pos) = do
      f' <- lookupSym' f $ Just pos
      (pts, rt) <- lookupFun f' $ Just pos
      (params', body' ::: body_ty) <-
        withParams params $ \params' -> (params',) <$> checkExp body
      compatTypes pos body_ty rt
      mrt' <-
        case mrt of
          Nothing -> pure Nothing
          Just (rt_s, pos) -> do
            rt_sym <- lookupSym' rt_s $ Just pos
            pure $ Just (rt_sym, pos)
      pure $ AST.FunDec f' (map deannotate params') mrt' body' pos

    withParams :: [UntypedField] -> ([Field ::: Ty] -> TypeCheckM frame a) -> TypeCheckM frame a
    withParams ps m = withParams' ps []
      where
        withParams' [] fs' = m $ reverse fs'
        withParams' (AST.Field field ty_s pos : fs) fs' =
          withSym field $ \field' -> do
            ty_sym <- lookupSym' ty_s $ Just pos
            ty <- lookupTy ty_sym $ Just pos
            insertSym field' (VarEntry undefined ty) $ withParams' fs (AST.Field field' ty_sym pos ::: ty : fs') -- fix
checkDec (VarDec s mty e pos) m = do
  e' ::: e_ty <- checkExp e
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
      compatTypes ty_pos e_ty ty
      pure $ Just (ty_sym, ty_pos)
  withSym s $ \sym ->
    insertSym
      sym
      (VarEntry undefined e_ty) -- fix
      (m $ VarDec sym mtyt e' pos)
checkDec (TypeDec decs) m =
  withHeaders decs
  where
    withHeaders [] = checkTypeDecs decs []
    withHeaders ((s, sty, pos) : ds) = do
      withSym s $ \sym ->
        insertSym sym (Name sym) (withHeaders ds)

    checkTypeDecs [] ds' = do
      checkTyTable
      m $ TypeDec $ reverse ds'
    checkTypeDecs ((s, sty, pos) : ds) ds' = do
      sym <- lookupSym' s $ Just pos
      checkTy sty $ \(sty' ::: ty) ->
        insertSym sym ty $
          checkTypeDecs ds $
            (sym, sty', pos) : ds'

    checkTyTable :: TypeCheckM frame ()
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

    checkTy :: UntypedTy -> (AST.Ty Symbol ::: Ty -> TypeCheckM frame a) -> TypeCheckM frame a
    checkTy (NameTy s pos) m = do
      sym <- lookupSym' s $ Just pos
      ty <- lookupTyNoUnpack sym $ Just pos
      m $ NameTy sym pos ::: ty
    checkTy (RecordTy fields) m = do
      withFields fields [] $ \fields' -> do
        tag <- newTag
        let ty_fields = map (\(AST.Field sym _ _ ::: ty) -> (sym, ty)) fields'
            ty = Record ty_fields tag
        m $ RecordTy (map deannotate fields') ::: ty
      where
        withFields ::
          [UntypedField] ->
          [Field ::: Ty] ->
          ([Field ::: Ty] -> TypeCheckM frame a) ->
          TypeCheckM frame a
        withFields [] fs' n =
          n $ reverse fs'
        withFields (f : fs) fs' n =
          checkField f $ \f' ->
            withFields fs (f' : fs') n

        checkField :: UntypedField -> (Field ::: Ty -> TypeCheckM frame a) -> TypeCheckM frame a
        checkField (AST.Field field ty_s pos) n =
          withSym field $ \field_sym -> do
            ty_sym <- lookupSym' ty_s $ Just pos
            ty <- lookupTyNoUnpack ty_sym $ Just pos
            n $ AST.Field field_sym ty_sym pos ::: ty
    checkTy (ArrayTy s pos) m = do
      sym <- lookupSym' s $ Just pos
      ty <- lookupTyNoUnpack sym $ Just pos
      tag <- newTag
      m $ ArrayTy sym pos ::: Array ty tag

compatTypes :: SourcePos -> Ty -> Ty -> TypeCheckM frame ()
compatTypes pos t1 t2 =
  unless (okTypes t1 t2) $
    throwError $
      InvalidType t1 (S.singleton t2) pos
  where
    okTypes Record {} Nil = True
    okTypes Nil Record {} = True
    okTypes t1 t2 = t1 == t2
