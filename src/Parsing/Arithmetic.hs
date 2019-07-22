module Parsing.Arithmetic
  ( AExpr,
      expr,
  ) where

import Parsing.Combinators

data Op = Add | Mult deriving Show

data AExpr =
    Binary Op AExpr AExpr
  | Operand Double deriving Show

expr :: Parser AExpr
expr = add

add :: Parser AExpr
add = chainl1 mult addOp

addOp :: Parser (AExpr -> AExpr -> AExpr)
addOp = binaryOperator "+" (Binary Add)

mult :: Parser AExpr
mult = chainl1 primary multOp

multOp :: Parser (AExpr -> AExpr -> AExpr)
multOp = binaryOperator "*" (Binary Mult)

primary :: Parser AExpr
primary = double

double :: Parser AExpr
double = Operand . read <$> doubleStr
