module Evaluation.AlphaConversion
    (
      alphaConvert
    ) where

import Parsing.LambdaExpressions

alphaConvert :: String -> String -> Expr -> Expr
alphaConvert from to (App left right) =
    App (alphaConvert from to left) (alphaConvert from to right)
--alphaConvert from to (Grouping expr) = Grouping $ alphaConvert from to expr
alphaConvert from to var@(Var x) = if x == from then Var to else var
alphaConvert from to (Lambda var expr)
  | var == from = Lambda to $ alphaConvert from to expr
  | otherwise = Lambda var $ alphaConvert from to expr
alphaConvert _ _ _ = error "Alphaconversion of meta variable"
