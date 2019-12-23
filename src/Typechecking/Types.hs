{-# LANGUAGE LambdaCase #-}
module Typechecking.Types (Type(..), Types(..), Substitution, compose, newTVar, TI) where

import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State

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
    TArrow l r -> "(" ++ show l ++ " -> " ++ show r ++ ")"

type Substitution = Map.Map String Type

-- Left-to-right composition
compose :: Substitution -> Substitution -> Substitution
compose f g = let composed = Map.map (substitute f) g in Map.union composed f

class Types a where
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