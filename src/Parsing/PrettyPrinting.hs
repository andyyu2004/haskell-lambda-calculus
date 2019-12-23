module Parsing.PrettyPrinting
  (
    parenthesized,
    nospaces,
    formatExpr
  ) where

import Parsing.LambdaExpressions(Expr(..))
import Text.Printf

parenthesized :: Expr -> String
--parenthesized (Grouping expr) = printf "(%s)" $ parenthesized expr
parenthesized (Lambda var expr) = printf "(λ%s.(%s))" var $ parenthesized expr
parenthesized (App left right) = printf "(%s) (%s)" (parenthesized left) (parenthesized right)
parenthesized (Var var) = printf "%s" var
parenthesized (Binding name expr) = printf "%s <- %s" name $ parenthesized expr
parenthesized (Metavariable name) = printf "%s" name
parenthesized (Let name expr body) = printf "let %s = %s in %s" name (parenthesized expr) (parenthesized body)
parenthesized (EBool b) = show b
parenthesized (EInt b) = show b

nospaces :: Expr -> String
--formatExpr (Grouping expr) = printf "(%s)" $ formatExpr expr
nospaces (Var var) = printf "%s" var
nospaces (Binding name expr) = printf "%s <- %s" name $ nospaces expr
nospaces expr@(Lambda _ _) = printf "λ%s" $ formatAbstraction expr
nospaces (Metavariable name) = printf "%s" name
nospaces (Let name expr body) = printf "let %s = %s in %s" name (nospaces expr) (nospaces body)
nospaces (EBool b) = show b
nospaces (EInt b) = show b
nospaces (App left right) = case right of
    -- Required parentheses on right side in right associative application
    -- Require parentheses on left when left is abstraction
    App _ _ -> case left of
        Lambda _ _ -> printf "(%s)(%s)" (nospaces left) (nospaces right)
        _               -> printf "%s(%s)" (nospaces left) (nospaces right)
    _               -> case left of
        Lambda _ _ -> printf "(%s)%s" (nospaces left) (nospaces right)
        _               -> printf "%s%s" (nospaces left) (nospaces right)

-- Requires another function to prevent lots of lambdas in resulting string
formatAbstraction :: Expr -> String
formatAbstraction (Lambda var expr) = case expr of
    Lambda _ _ -> printf "%s%s" var $ formatAbstraction expr
    _               -> printf "%s.%s" var $ nospaces expr
formatAbstraction _ = error "Formatting non abstraction in formatAbstraction"

formatExpr :: Expr -> String
--formatExpr (Grouping expr) = printf "(%s)" $ formatExpr expr
formatExpr (Var var) = printf "%s" var
formatExpr (Binding name expr) = printf "%s <- %s" name $ formatExpr expr
formatExpr expr@(Lambda _ _) = printf "λ%s" $ formatAbstraction' expr
formatExpr (Metavariable name) = printf "%s" name
formatExpr (Let name expr body) = printf "let %s = %s in %s" name (formatExpr expr) (formatExpr body)
formatExpr (EBool b) = show b
formatExpr (EInt b)  = show b
formatExpr (App left right) = case right of
    -- Required parentheses on right side in right associative application
    -- Require parentheses on left when left is abstraction
    App _ _ -> case left of
        Lambda _ _ -> printf "(%s) (%s)" (formatExpr left) (formatExpr right)
        _               -> printf "%s (%s)" (formatExpr left) (formatExpr right)
    _               -> case left of
        Lambda _ _ -> printf "(%s) %s" (formatExpr left) (formatExpr right)
        _               -> printf "%s %s" (formatExpr left) (formatExpr right)

formatAbstraction' :: Expr -> String
formatAbstraction' (Lambda var expr) = case expr of
    Lambda _ _ -> printf "%s%s" var $ formatAbstraction' expr
    _               -> printf "%s.%s" var $ formatExpr expr
formatAbstraction' _ = error "Formatting non abstraction in formatAbstraction"











