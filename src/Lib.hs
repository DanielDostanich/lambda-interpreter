{-# LANGUAGE FlexibleContexts #-}
module Lib where

import Data.Char
import Data.Maybe
import Data.Bifunctor
import Control.Monad.Except
import Data.List
infixl 4 :@
infixr 3 :->

type Symb = String 

data Expr = Var Symb 
          | Expr :@ Expr
          | Lam Symb Expr
  deriving (Eq,Show)

data Type = TVar Symb 
          | Type :-> Type
  deriving (Eq,Show)

newtype Env = Env [(Symb,Type)]
  deriving (Eq,Show)

newtype SubsTy = SubsTy [(Symb, Type)]
  deriving (Eq,Show)

freeVars :: Expr -> [Symb]
freeVars (Var s) = [s]
freeVars (e1 :@ e2) = union (freeVars e1) (freeVars e2)
freeVars (Lam s e) = filter (/= s) (freeVars e)

freeTVars :: Type -> [Symb]
freeTVars (TVar s) = [s]
freeTVars (t1 :-> t2) = union (freeTVars t1)  (freeTVars t2)

extendEnv :: Env -> Symb -> Type -> Env
extendEnv (Env env) s t = 
    case lookup s env of 
        Nothing -> Env $ (s, t) : env
        Just _ -> Env env

freeTVarsEnv :: Env -> [Symb]
freeTVarsEnv (Env env) = foldr (union . (freeTVars . snd)) [] env

appEnv :: MonadError String m => Env -> Symb -> m Type
appEnv (Env env) var =
  case lookup var env of
    Nothing -> throwError $ "There is no variable " ++ show var ++ " in the enviroment."
    Just t -> pure t

appSubsTy :: SubsTy -> Type -> Type
appSubsTy (SubsTy subs) (TVar var) = Data.Maybe.fromMaybe (TVar var) $ lookup var subs
appSubsTy s (t1 :-> t2) = appSubsTy s t1 :-> appSubsTy s t2

appSubsEnv :: SubsTy -> Env -> Env
appSubsEnv (SubsTy subs) (Env env) = Env $ map (Data.Bifunctor.second (appSubsTy (SubsTy subs))) env

composeSubsTy :: SubsTy -> SubsTy -> SubsTy
composeSubsTy (SubsTy s1) (SubsTy s2) =
  let unSubs = map (\s -> (s, TVar s)) $ union (map fst s1) (map fst s2) in
  SubsTy $ (\ subs (s,t) -> (s, appSubsTy (SubsTy subs) t)) s1 <$> ((\ subs (s,t) -> (s, appSubsTy (SubsTy subs) t)) s2 <$> unSubs)


instance Semigroup SubsTy where
  (<>) = composeSubsTy

instance Monoid SubsTy where
  mempty = SubsTy []

unify :: MonadError String m => Type -> Type -> m SubsTy
unify (TVar var1) (TVar var2) = if var1 == var2 then return $ SubsTy [] else return $ SubsTy [(var1, TVar var2)]
unify (TVar var) t =
    if var `elem` freeTVars t 
        then throwError $ "Can't unify (" ++ show (TVar var) ++ ") with (" ++ show t ++ ")!"
        else return $ SubsTy [(var, t)]
unify (t1 :-> t2) (TVar s) = unify (TVar s) (t1 :-> t2)
unify (t1 :-> t2) (t1' :-> t2') = do
  union2 <- unify t2 t2'
  union1 <- unify (appSubsTy union2 t1) (appSubsTy union2 t1')
  return $ union1 <> union2



equations :: MonadError String m => Env -> Expr -> Type -> m [(Type,Type)]
equations env e t = do
  insider env e t listOfChar
  where
    freeT = freeTVarsEnv env `union` freeTVars t
    listOfChar = map ((:[]) . chr) [97..10000]  \\ freeT
    insider :: MonadError String m => Env -> Expr -> Type -> [Symb] -> m [(Type,Type)]
    insider env (Var x) t _ = do
        t' <- appEnv env x
        return [(t, t')]
    insider env (m :@ n) t l = do
        let a = TVar $ head l
        res1 <- insider env m (a :-> t) (tail l)
        let newL = tail l \\ foldr (union . (\(t1,t2) -> freeTVars t1 `union` freeTVars t2)) [] res1
        res2 <- insider env n a newL
        return $ res1 ++ res2
    insider e (Lam x m) t l = do
        let a = TVar $ head l
        let b = TVar $ head (tail l)
        res1 <- insider (extendEnv e x a) m b (tail $ tail l)
        let res2 = [(a :-> b, t)]
        return $ res2 ++ res1


principlePair :: MonadError String m => Expr -> m (Env,Type)
principlePair m = do
  let freeV = freeVars m
  let env = Env $ zip freeV types
  let t = types !! length freeV
  e <- equations env m t
  s <- fun (reverse e)
  return (appSubsEnv s env, appSubsTy s t)
  where
    fun [] = return mempty
    fun [(t1,t2)] = unify t1 t2
    fun ((t1,t2):es) = do
      s1 <- unify t1 t2
      let newEs = map (bimap (appSubsTy s1) (appSubsTy s1)) es
      fun newEs
    types = map (TVar . (:[]) . chr) [97..10000]