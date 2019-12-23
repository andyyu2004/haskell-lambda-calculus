{-# LANGUAGE LambdaCase #-}
module Typechecking.TypedExpr(TypedExpr(..), getType) where

import Typechecking.Types(Type(..))

-- Each expression and each binder within each expression has an associated type
data TypedExpr
  = TEInt Type Int
  | TEBool Type Bool
  | TELambda Type (Type, String) TypedExpr
  | TEApp Type TypedExpr TypedExpr
  | TELet Type (Type, String) TypedExpr TypedExpr
  | TEVar Type String
  deriving (Eq, Show)

getType :: TypedExpr -> Type
getType = \case
  TEInt  t _     -> t
  TEBool t _     -> t
  TELambda t _ _ -> t
  TEApp t _ _    -> t
  TELet t _ _ _  -> t
  TEVar t _      -> t


