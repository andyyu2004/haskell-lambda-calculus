module Evaluation.Substitution ( substitute, isFree ) where

import Parsing.LambdaExpressions(Expr(..), varNames)
import Evaluation.AlphaConversion
--import Debug.Trace(trace)
--import Text.Printf(printf)

-- Substitutes variable var with the expr 'with' in expression 'expression'
substitute :: Expr -> Char -> Expr -> Expr
substitute with var expression = case expression of
    Application left right -> Application (sub left) (sub right)
    Variable x -> if x == var then with else expression
-- if the abstraction variable name is free in the substitution, the free var becomes bound => rename
-- Rename the bound variable
    Abstraction name expr
        -- Do not substitute bound variables
        | name == var -> expression
        | isFree name with -> let newname = generateName varNames expr in -- by definition I think expr should be with, but somethings go wrong
            Abstraction newname $ sub $ alphaConvert name newname expr
        | otherwise -> Abstraction name $ sub expr
    _ -> error "Attempted substitution of binding or metavariable"
  where sub = substitute with var

-- A variable x is free if it is not bound in the body of an abstraction
isFree :: Char -> Expr -> Bool
isFree var expression = case expression of
    Application left right -> free left || free right
    Variable name          -> var == name
    Abstraction name expr  -> name /= var && free expr
    _                      -> error "Checking if metavariable or binding is free"
  where free = isFree var

-- Where string is a list of chars of valid variable names
generateName :: String -> Expr -> Char
generateName [] _ = error "Not enough variable names"
generateName (x:xs) expr
  | not $ isFree x expr = x
  | otherwise = generateName xs expr