{-# LANGUAGE LambdaCase #-}
module Typechecking.Types (Type(..), Types(..), Substitution, compose, newTVar, TI, normTVars) where

import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import Debug.Trace(trace)

data Type
  = TInt
  | TBool
  | TVar String
  | TArrow Type Type
    deriving Eq

instance Show Type where
  show = \case
    TInt       -> "Int"
    TBool      -> "Bool"
    TVar t     -> t
    TArrow l r -> case l of
      TArrow _ _ -> "(" ++ show l ++ ")" ++ " -> " ++ show r
      _          -> show l ++ " -> " ++  show r

type Substitution = Map.Map String Type

-- Renames type variables from a-z a_i-z_i in order of appearance
normTVars :: Type -> Type
normTVars t = evalState (normalizeTypeVariables t)  (Map.empty, map (:[]) ['a'..'z'], 0, 0)

normalizeTypeVariables :: Type -> State (Map.Map String String, [String], Int, Int) Type
normalizeTypeVariables = \case
  TInt       -> return TInt
  TBool      -> return TBool
  TArrow l r -> TArrow <$> normalizeTypeVariables l <*> normalizeTypeVariables r
  TVar t     -> do
    (vars, xs, i, n) <- get
    case Map.lookup t vars of
      Just var -> return $ TVar var
      Nothing -> do
        let var = if n == 0 then xs !! i else xs !! i ++ show n
        let newmap = Map.insert t var vars in
          if i == length xs - 1 then put (newmap, xs, 0, n + 1) else put (newmap, xs, i + 1, n)
        return $ TVar var

-- Left-to-right composition
-- Map.union is left-biased!
compose :: Substitution -> Substitution -> Substitution
compose f g = let composed = Map.map (substitute g) f in Map.union composed g

class Types a where
  -- Apply substitution
  substitute :: Substitution -> a -> a
  ftv :: a -> Set.Set String


instance Types a => Types [a] where
  substitute s = map $ substitute s
  ftv = Set.unions . map ftv

instance Types Type where
  ftv = \case
    TVar t     -> Set.singleton t
    TInt       -> Set.empty
    TBool      -> Set.empty
    TArrow l r -> Set.union (ftv l) (ftv r)

  substitute s ty = case ty of
    TArrow l r -> TArrow (sub l) (sub r)
    x@(TVar t) -> fromMaybe x $ Map.lookup t s
    TInt       -> TInt
    TBool      -> TBool
    where sub = substitute s

type TI a = State Int a

newTVar :: TI Type
newTVar = do
  i <- get
  put (i + 1)
  return $ TVar $ "t" ++ show i