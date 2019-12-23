{-# LANGUAGE LambdaCase #-}
module Typechecking.Inference (infer, runInference, bindTypeVar) where

import Typechecking.Scheme(Ctx(..), Scheme(..), instantiate)
import Typechecking.Types(Type(..), Substitution, substitute, compose, TI, newTVar, ftv)
import Parsing.LambdaExpressions(Expr(..))
import qualified Data.Map as Map
import Control.Monad.State (evalState)
import qualified Data.Set as Set

-- Calculates the mgu
unify :: Type -> Type -> TI Substitution
unify x y | x == y = return Map.empty
unify (TArrow a b) (TArrow t u) = do
  s  <- unify a t
  s' <- unify (substitute s b) (substitute s u)
  return $ compose s s'
unify (TVar x) t = bindTypeVar x t
unify t (TVar x) = bindTypeVar x t
unify t u = error $ "Failed to unify" ++ show t ++ show u

bindTypeVar :: String -> Type ->  TI Substitution
bindTypeVar var t
  | TVar var == t = return Map.empty
  | Set.member var $ ftv t = error "occurs check failed"
  | otherwise = return $ Map.singleton var t

runInference :: Expr -> Type
runInference expr = let (sub, t) = evalState stateop 0 in substitute sub t
    where stateop = infer (Ctx Map.empty) expr

infer :: Ctx -> Expr -> TI (Substitution, Type)
infer (Ctx ctx) = \case
    Lambda binder body -> do
      tvar <- newTVar
      let extendedctx = Ctx $ Map.insert binder (Scheme [] tvar) ctx
      (s, tbody) <- infer extendedctx body
      return (s, TArrow (substitute s tvar) tbody)
    Let name expr body -> do
      (s, texpr) <- infer (Ctx ctx) expr
      let extendedctx = Ctx $ Map.insert name (Scheme [] texpr) ctx
      (s', tbody) <- infer (substitute s extendedctx) body
      return (compose s s', tbody)
    Var x -> case Map.lookup x ctx of
      Nothing -> error "Unbound type variable"
      Just scheme -> instantiate scheme >>= \t -> return (Map.empty, t)
    App f x -> do
      (s0, tf) <- infer (Ctx ctx) f
      (s1, tx) <- infer (substitute s0 $ Ctx ctx) x
      tret     <- newTVar
      s2       <- unify (substitute s1 tf) $ TArrow tx tret
      -- Note that in texts, this is often in the reverse order to the ones above for some reason (doesn't seem right)
      let composed = s0 `compose` s1 `compose` s2
      return (composed, substitute s2 tret)
    EInt _  -> return (Map.empty, TInt)
    EBool _ -> return (Map.empty, TBool)



