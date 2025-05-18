{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Semant
  ( transProg,
    (:::) (..),
    Var,
    Exp,
    Dec,
    Field,
  )
where

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
import Frame (Frame, X86)
import Symbol
import Temp qualified
import Translate qualified
import Types

type Var = AST.Var Symbol

type Exp = AST.Exp Symbol

type Dec = AST.Dec Symbol

type Field = AST.Field Symbol

data Env frame = Env
  { envVal :: SymTable (EnvEntry frame),
    envTy :: SymTable Ty,
    envSym :: Map String Symbol,
    envLevel :: Translate.Level frame
  }

deriving instance (Show frame, Show (EnvEntry frame)) => Show (Env frame)

deriving instance (Eq frame, Eq (EnvEntry frame)) => Eq (Env frame)

deriving instance (Ord frame, Ord (EnvEntry frame)) => Ord (Env frame)

initEnv :: (Frame frame) => Env frame
initEnv =
  Env
    { envVal = val_tys,
      envTy = ty_tys,
      envSym = val_syms <> ty_syms,
      envLevel = Translate.outermost
    }
  where
    ((val_syms, val_tys), (ty_syms, ty_tys)) = prelude

data Error
  = InvalidType Ty (Set Ty) SourcePos
  | Undefined String (Maybe SourcePos)
  | Error String (Maybe SourcePos)
  deriving (Show, Eq)

newtype TransM frame a = TransM {runTransM :: ExceptT Error (RWS (Env frame) () Tag) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader (Env frame),
      MonadWriter (),
      MonadState Tag,
      MonadError Error
    )

instance (Frame frame) => MonadSymTable (TransM frame) (EnvEntry frame) where
  askSymTable = asks envVal
  withSymTable f = local $ \env -> env {envVal = f $ envVal env}

instance (Frame frame) => MonadSymTable (TransM frame) Ty where
  askSymTable = asks envTy
  withSymTable f = local $ \env -> env {envTy = f $ envTy env}

data (:::) a b = a ::: b

deannotate :: (a ::: b) -> a
deannotate (a ::: _) = a

typeOf :: (a ::: b) -> b
typeOf (_ ::: b) = b

newLevel ::
  (Frame frame) =>
  String ->
  [Translate.Escape] ->
  TransM frame (Translate.Level frame, Temp.Label)
newLevel s escapes = do
  l <- Temp.namedLabel s
  lvl <- asks envLevel
  pure (Translate.newLevel lvl l escapes, l)

allocLocal :: (Frame frame) => Translate.Escape -> TransM frame (Translate.Access frame)
allocLocal escape = do
  lvl <- asks envLevel
  pure $ Translate.allocLocal lvl escape

lookupSym :: (Frame frame) => String -> TransM frame (Maybe Symbol)
lookupSym s = (M.!? s) <$> asks envSym

lookupSym' :: (Frame frame) => String -> Maybe SourcePos -> TransM frame Symbol
lookupSym' s pos = do
  msym <- lookupSym s
  case msym of
    Nothing -> throwError $ Undefined s pos
    Just sym -> pure sym

lookupVar :: forall frame. (Frame frame) => Symbol -> Maybe SourcePos -> TransM frame Ty
lookupVar sym pos = do
  (mentry :: Maybe (EnvEntry frame)) <- askSym sym
  case mentry of
    Just (VarEntry _ ty) -> pure ty
    _ -> throwError $ Undefined (symName sym) pos

lookupFun :: forall frame. (Frame frame) => Symbol -> Maybe SourcePos -> TransM frame ([Ty], Ty)
lookupFun sym pos = do
  (mentry :: Maybe (EnvEntry frame)) <- askSym sym
  case mentry of
    Just (FunEntry _ _ pts rt) -> pure (pts, rt)
    _ -> throwError $ Undefined (symName sym) pos

lookupTy :: (Frame frame) => Symbol -> Maybe SourcePos -> TransM frame Ty
lookupTy sym pos = do
  mty <-
    runMaybeT $
      MaybeT . unpack =<< MaybeT (askSym sym)
  case mty of
    Nothing -> throwError $ Undefined (symName sym) Nothing
    Just ty -> pure ty

lookupTyNoUnpack :: (Frame frame) => Symbol -> Maybe SourcePos -> TransM frame Ty
lookupTyNoUnpack sym pos = do
  mty <- askSym sym
  case mty of
    Nothing -> throwError $ Undefined (symName sym) pos
    Just ty -> pure ty

withSym :: (Frame frame) => String -> (Symbol -> TransM frame a) -> TransM frame a
withSym s m = do
  mname <- (M.!? s) <$> asks envSym
  case mname of
    Just sym -> m sym
    Nothing -> do
      sym <- newSym s
      local (\env -> env {envSym = M.insert s sym $ envSym env}) $ m sym

transProg :: UntypedExp -> Either Error (Exp ::: Ty)
transProg e = fst $ (evalRWS $ runExceptT $ runTransM $ (transExp @X86) e) initEnv preludeTag

transVar :: (Frame frame) => UntypedVar -> TransM frame (Var ::: Ty)
transVar (SimpleVar s pos) = do
  v <- lookupSym' s $ Just pos
  ty <- lookupVar v $ Just pos
  pure $ SimpleVar v pos ::: ty
transVar (FieldVar v field pos) = do
  v' ::: v_t <- transVar v
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
transVar (SubscriptVar v i pos) = do
  v' ::: v_t <- transVar v
  i' ::: i_t <- transExp i
  case elemType v_t of
    Nothing ->
      throwError $
        Error ("Type " <> show v_t <> " isn't an array.") $
          Just pos
    Just e_t -> do
      compatTypes pos i_t Int
      pure $ SubscriptVar v' i' pos ::: e_t

transExp :: (Frame frame) => UntypedExp -> TransM frame (Exp ::: Ty)
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
  void $ zipWithM (compatTypes pos) (map typeOf args') pts
  pure $ CallExp f' (map deannotate args') pos ::: rt
transExp (OpExp l op r pos) = do
  lt@(l' ::: l_ty) <- transExp l
  rt@(r' ::: r_ty) <- transExp r
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
    check :: (a ::: Ty) -> [Ty] -> SourcePos -> TransM frame Ty
    check a tys pos
      | typeOf a `elem` tys = pure $ typeOf a
      | otherwise = throwError $ InvalidType (typeOf a) (S.fromList tys) pos
transExp (RecordExp t fields pos) = do
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
              e' ::: e_ty <- transExp e
              compatTypes pos e_ty f_ty'
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
  compatTypes pos e_t v_t
  pure $ AssignExp v' e' pos ::: Unit
transExp (IfExp c t mf pos) = do
  c' ::: c_t <- transExp c
  compatTypes pos c_t Int
  t' ::: t_t <- transExp t
  mf' <- case mf of
    Nothing -> do
      compatTypes pos t_t Unit
      pure Nothing
    Just f -> do
      f' ::: f_t <- transExp f
      compatTypes pos t_t f_t
      pure $ Just f'
  pure $ IfExp c' t' mf' pos ::: t_t
transExp (WhileExp c b pos) = do
  c' ::: c_t <- transExp c
  compatTypes pos c_t Int
  b' ::: b_t <- transExp b
  compatTypes pos b_t Unit
  pure $ WhileExp c' b' pos ::: b_t
transExp (ForExp i from to b pos) = do
  from' ::: from_t <- transExp from
  compatTypes pos from_t Int
  to' ::: to_t <- transExp to
  compatTypes pos to_t Int
  withSym i $ \i_sym -> do
    access <- allocLocal True
    b' ::: b_t <- insertSym i_sym (VarEntry access Int) $ transExp b
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
      compatTypes pos n_t Int
      e' ::: e_t <- transExp e
      compatTypes pos e_t elem_t
      pure $ ArrayExp t_sym n' e' pos ::: t'

transDecs :: (Frame frame) => [UntypedDec] -> ([Dec] -> TransM frame a) -> TransM frame a
transDecs ds m = transDecs' ds []
  where
    transDecs' [] ds' = m $ reverse ds'
    transDecs' (d : ds) ds' =
      transDec d $ \d' ->
        transDecs' ds (d' : ds')

transDec ::
  forall frame a.
  (Frame frame) =>
  UntypedDec ->
  (Dec -> TransM frame a) ->
  TransM frame a
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
      withSym f $ \f' -> do
        (lvl, l) <- newLevel f (map (const True) params)
        insertSym f' (FunEntry lvl l (map typeOf params') body_ty) $
          withHeaders ds

    transFunDec (AST.FunDec f params mrt body pos) = do
      f' <- lookupSym' f $ Just pos
      (pts, rt) <- lookupFun f' $ Just pos
      (params', body' ::: body_ty) <-
        withParams params $ \params' -> (params',) <$> transExp body
      compatTypes pos body_ty rt
      mrt' <-
        case mrt of
          Nothing -> pure Nothing
          Just (rt_s, pos) -> do
            rt_sym <- lookupSym' rt_s $ Just pos
            pure $ Just (rt_sym, pos)
      pure $ AST.FunDec f' (map deannotate params') mrt' body' pos

    withParams ::
      forall a.
      [UntypedField] ->
      ([Field ::: Ty] -> TransM frame a) ->
      TransM frame a
    withParams ps m = withParams' ps []
      where
        withParams' [] fs' = m $ reverse fs'
        withParams' (AST.Field field ty_s pos : fs) fs' =
          withSym field $ \field' -> do
            ty_sym <- lookupSym' ty_s $ Just pos
            ty <- lookupTy ty_sym $ Just pos
            access <- allocLocal True
            insertSym field' (VarEntry access ty) $
              withParams' fs (AST.Field field' ty_sym pos ::: ty : fs')
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
      compatTypes ty_pos e_ty ty
      pure $ Just (ty_sym, ty_pos)
  withSym s $ \sym -> do
    access <- allocLocal True
    insertSym
      sym
      (VarEntry access e_ty)
      (m $ VarDec sym mtyt e' pos)
transDec (TypeDec decs) m =
  withHeaders decs
  where
    withHeaders [] = transTypeDecs decs []
    withHeaders ((s, sty, pos) : ds) = do
      withSym s $ \sym ->
        insertSym sym (Name sym) (withHeaders ds)

    transTypeDecs [] ds' = do
      transTyTable
      m $ TypeDec $ reverse ds'
    transTypeDecs ((s, sty, pos) : ds) ds' = do
      sym <- lookupSym' s $ Just pos
      transTy sty $ \(sty' ::: ty) ->
        insertSym sym ty $
          transTypeDecs ds $
            (sym, sty', pos) : ds'

    transTyTable :: TransM frame ()
    transTyTable =
      void $ traverse transTy =<< asks envTy
      where
        transTy (Name sym) = do
          mt <- askSym sym
          case mt of
            Nothing ->
              error $ "transTy: error, unknown type: " <> show sym
            Just t@(Name {}) ->
              throwError $ Error ("recursive cycle involving " <> show t) Nothing
            _ -> pure ()
        transTy _ = pure ()

    transTy :: UntypedTy -> (AST.Ty Symbol ::: Ty -> TransM frame a) -> TransM frame a
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
          ([Field ::: Ty] -> TransM frame a) ->
          TransM frame a
        withFields [] fs' n =
          n $ reverse fs'
        withFields (f : fs) fs' n =
          transField f $ \f' ->
            withFields fs (f' : fs') n

        transField :: UntypedField -> (Field ::: Ty -> TransM frame a) -> TransM frame a
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

compatTypes :: SourcePos -> Ty -> Ty -> TransM frame ()
compatTypes pos t1 t2 =
  unless (okTypes t1 t2) $
    throwError $
      InvalidType t1 (S.singleton t2) pos
  where
    okTypes Record {} Nil = True
    okTypes Nil Record {} = True
    okTypes t1 t2 = t1 == t2
