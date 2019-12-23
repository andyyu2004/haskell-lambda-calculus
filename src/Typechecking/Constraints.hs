{-# LANGUAGE LambdaCase #-}
module Typechecking.Constraints (
   TConstraint(..),
   runAnnotation,
   genConstraints,
   runInferenceC,
) where

import Typechecking.Types
import Typechecking.TypedExpr(TypedExpr(..), getType)
import Typechecking.Inference(bindTypeVar)
import Parsing.LambdaExpressions
import Typechecking.Scheme
import qualified Data.Map as Map
import Control.Monad.State
-- Constraint based type inference

-- Type constraint grammar
data TConstraint
  = CEq Type Type
  | CAnd TConstraint TConstraint
  | CEmpty
  deriving Show

instance Types TConstraint where
   substitute s = \case
    CEq t u  -> CEq (substitute s t) (substitute s u)
    CAnd x y -> CAnd (substitute s x) (substitute s y)
    CEmpty    -> CEmpty


runAnnotation :: Expr -> TypedExpr
runAnnotation expr = evalState (annotate (Ctx Map.empty) expr) 0

-- Annotate every expression with a new type variable
annotate :: Ctx -> Expr -> TI TypedExpr
annotate ctx = \case
  EBool b         -> return $ TEBool TBool b
  EInt i          -> return $ TEInt TInt i
  Lambda var expr -> do
    tvar     <- newTVar
    tlambda  <- newTVar
    let extended = Ctx $ let Ctx m = ctx in Map.insert var (Scheme [] tvar) m
    TELambda tlambda (tvar, var) <$> annotate extended expr
  App l r -> TEApp <$> newTVar <*> annotate ctx l <*> annotate ctx r
  Let var expr body -> do
    tvar  <- newTVar
    let extended = Ctx $ let Ctx m = ctx in Map.insert var (Scheme [] tvar) m
    texpr <- annotate extended expr
    tbody <- annotate extended body
    tlet  <- newTVar
    return $ TELet tlet (tvar, var) texpr tbody
  Var name -> let Ctx m = ctx in case Map.lookup name m of
    Just t  -> instantiate t >>= \ty -> return $ TEVar ty name
    Nothing -> error $ "unbound variable " ++ name


genConstraints :: TypedExpr -> TConstraint
genConstraints = \case
  TEInt  t _                -> CEq t TInt
  TEBool t _                -> CEq t TBool
  TELambda t (tvar, _) expr ->
    let cexpr   = genConstraints expr
        clambda = CEq t $ TArrow tvar $ getType expr
    in CAnd cexpr clambda
  TEApp t f arg -> let capp = CEq (getType f) $ TArrow (getType arg) t in
    genConstraints f `CAnd` genConstraints arg `CAnd`capp
  TEVar _ _ -> CEmpty
  TELet t (tvar, _) expr body ->
    let cexpr = genConstraints expr
        cbody = genConstraints body
        cret = CEq t (getType body)
        cvar = CEq tvar (getType expr)
    in cexpr `CAnd` cbody `CAnd` cret `CAnd` cvar


unifyConstraint :: TConstraint -> TI Substitution
unifyConstraint = \case
  CEmpty                        -> return Map.empty
  CEq TInt TInt                 -> return Map.empty
  CEq TBool TBool               -> return Map.empty
  CEq (TArrow a b) (TArrow t u) -> unifyConstraint $ CEq a t `CAnd` CEq b u
  CEq (TVar x) t                -> bindTypeVar x t
  CEq t (TVar x)                -> bindTypeVar x t
  CEq t u                       -> error $ "failed to unify " ++ show t ++ " with " ++ show u
  CAnd x y                      -> do
    s   <- unifyConstraint x
    s'  <- unifyConstraint $ substitute s y
    return $ compose s s'

runInferenceC :: Expr -> Type
runInferenceC expr = evalState (inferc expr) 0

-- Inference using constraints
inferc :: Expr -> TI Type
inferc expr = do
  let annotated = runAnnotation expr
  let constraints = genConstraints annotated
  s <- unifyConstraint constraints
  return $ substitute s (getType annotated)





































