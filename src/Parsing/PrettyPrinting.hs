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
parenthesized (Abstraction var expr) = printf "(λ%s.(%s))" var $ parenthesized expr
parenthesized (Application left right) = printf "(%s) (%s)" (parenthesized left) (parenthesized right)
parenthesized (Variable var) = printf "%s" var
parenthesized (Binding name expr) = printf "%s <- %s" name $ parenthesized expr
parenthesized (Metavariable name) = printf "%s" name
parenthesized (Let name expr body) = printf "let %s = %s in %s" name (parenthesized expr) (parenthesized body)

nospaces :: Expr -> String
--formatExpr (Grouping expr) = printf "(%s)" $ formatExpr expr
nospaces (Variable var) = printf "%s" var
nospaces (Binding name expr) = printf "%s <- %s" name $ nospaces expr
nospaces expr@(Abstraction _ _) = printf "λ%s" $ formatAbstraction expr
nospaces (Metavariable name) = printf "%s" name
nospaces (Let name expr body) = printf "let %s = %s in %s" name (nospaces expr) (nospaces body)
nospaces (Application left right) = case right of
    -- Required parentheses on right side in right associative application
    -- Require parentheses on left when left is abstraction
    Application _ _ -> case left of
        Abstraction _ _ -> printf "(%s)(%s)" (nospaces left) (nospaces right)
        _               -> printf "%s(%s)" (nospaces left) (nospaces right)
    _               -> case left of
        Abstraction _ _ -> printf "(%s)%s" (nospaces left) (nospaces right)
        _               -> printf "%s%s" (nospaces left) (nospaces right)

-- Requires another function to prevent lots of lambdas in resulting string
formatAbstraction :: Expr -> String
formatAbstraction (Abstraction var expr) = case expr of
    Abstraction _ _ -> printf "%s%s" var $ formatAbstraction expr
    _               -> printf "%s.%s" var $ nospaces expr
formatAbstraction _ = error "Formatting non abstraction in formatAbstraction"

formatExpr :: Expr -> String
--formatExpr (Grouping expr) = printf "(%s)" $ formatExpr expr
formatExpr (Variable var) = printf "%s" var
formatExpr (Binding name expr) = printf "%s <- %s" name $ formatExpr expr
formatExpr expr@(Abstraction _ _) = printf "λ%s" $ formatAbstraction' expr
formatExpr (Metavariable name) = printf "%s" name
formatExpr (Let name expr body) = printf "let %s = %s in %s" name (formatExpr expr) (formatExpr body)
formatExpr (Application left right) = case right of
    -- Required parentheses on right side in right associative application
    -- Require parentheses on left when left is abstraction
    Application _ _ -> case left of
        Abstraction _ _ -> printf "(%s) (%s)" (formatExpr left) (formatExpr right)
        _               -> printf "%s (%s)" (formatExpr left) (formatExpr right)
    _               -> case left of
        Abstraction _ _ -> printf "(%s) %s" (formatExpr left) (formatExpr right)
        _               -> printf "%s %s" (formatExpr left) (formatExpr right)

formatAbstraction' :: Expr -> String
formatAbstraction' (Abstraction var expr) = case expr of
    Abstraction _ _ -> printf "%s%s" var $ formatAbstraction' expr
    _               -> printf "%s.%s" var $ formatExpr expr
formatAbstraction' _ = error "Formatting non abstraction in formatAbstraction"











