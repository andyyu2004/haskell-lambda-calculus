module Evaluation.AlphaConversion
    (
      alphaConvert
    ) where

import Parsing.LambdaExpressions

alphaConvert :: String -> String -> Expr -> Expr
alphaConvert from to (Application left right) =
    Application (alphaConvert from to left) (alphaConvert from to right)
--alphaConvert from to (Grouping expr) = Grouping $ alphaConvert from to expr
alphaConvert from to var@(Variable x) = if x == from then Variable to else var
alphaConvert from to (Abstraction var expr)
  | var == from = Abstraction to $ alphaConvert from to expr
  | otherwise = Abstraction var $ alphaConvert from to expr
alphaConvert _ _ _ = error "Alphaconversion of meta variable"
