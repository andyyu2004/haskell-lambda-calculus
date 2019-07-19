{-# LANGUAGE OverloadedStrings #-}

module Parsing.Arithmetic
  ( AExpr,
    expr,
  ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Parsing.Combinators
import Data.Text as T
import Data.Functor

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

binaryOperator :: T.Text -> (a -> a -> a) -> Parser (a -> a -> a)
binaryOperator opsymbol op = symbol opsymbol $> op

mult :: Parser AExpr
mult = chainl1 primary multOp

multOp :: Parser (AExpr -> AExpr -> AExpr)
multOp = binaryOperator "*" (Binary Mult)

primary :: Parser AExpr
primary = double

double :: Parser AExpr
double = Operand . read <$> doubleStr
