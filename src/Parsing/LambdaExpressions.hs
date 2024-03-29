module Parsing.LambdaExpressions
  (
    Expr (..),
    expression,
    desugarAbstraction,
    varNames,
    -- Exporting all for use in ghci
    variable,
    lambdaBinding,
    abstraction,
    primary,
    spacedword,
  ) where

import Parsing.Combinators
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Char
import Data.Text
import Text.Printf
import Data.Functor (($>))

data Expr
  = Var String
  | Lambda String Expr
  | App Expr Expr
  | Binding String Expr
  | Metavariable String
  | EBool Bool
  | EInt Int
  | Let String Expr Expr
--  | Grouping Expr (Unnecessary?)
    deriving Show

{-
  <expr> ::= <let>
  <let> ::= let ID = <expr> in <expr>
  <abstraction> ::= \<var>.<abstraction> | <application>
  <application> = <primary>  { < > <primary> }
  <primary> ::= <var> | ( <expr> )
  <var> ::= ID
-}

varNames :: String
varNames = ['a' .. 'z']


-- General function
desugarAbstraction :: String -> String
desugarAbstraction [] = []
desugarAbstraction (x:xs)
  | x == '\\' = '\\':desugarAbstraction' xs
  | otherwise = x:desugarAbstraction xs

-- When inside lambda abstraction
desugarAbstraction' :: String -> String
desugarAbstraction' [] = []
desugarAbstraction' [x] = [x]
desugarAbstraction' ts@(x:y:zs)
  | elem x varNames && y /= '.' = x:'.':'\\':desugarAbstraction' (y:zs)
  | otherwise = desugarAbstraction ts

metavariable :: Parser String
metavariable = (:) <$> (upperChar <|> numberChar) <*> many alphaNumChar

expression :: Parser Expr
expression = letexpr <* eof

-- Metavariable binding
-- Extra function so that let X = Y = E is valid instead of let X = let Y = E
-- Changed meta syntax to 'var' to account for the introduction on let bindings
-- letBinding :: Parser Expr
-- letBinding = ("var" >> spaceConsumer >> binding)
--              <|> abstraction

-- binding :: Parser Expr
-- binding = (Binding <$> metavariable <* spaceConsumer <*> (char '=' >> spaceConsumer >> abstraction))-- binding <|> abstraction))
--           <|> letexpr

letexpr :: Parser Expr
letexpr = (do
  _ <- spacedword "let"
  var  <- variable
  _ <- spacedword "="
  expr <- abstraction <* char '!' -- Cheap way to avoid abstraction eating too much, delimit with a !
  _ <- spacedword "in"
  Let var expr <$> abstraction) <|> abstraction

spacedword :: Text -> Parser Text
spacedword word = spaceConsumer *> string word <* spaceConsumer

abstraction :: Parser Expr
abstraction = lambdaBinding <*> abstraction <|> application

-- For primary parsing where the binding is already parsed. Otherwise infinite loop if abstraction is called directly
abstraction' :: (Expr -> Expr) -> Parser Expr
abstraction' var = (var <$> abstraction) <|> application

-- Parses the lambda and the variable, partially applies the Abstraction constructor
lambdaBinding :: Parser (Expr -> Expr)
lambdaBinding = Lambda <$> (lambda *> variable <* dot)

variable :: Parser String
variable = many $ satisfy isAsciiLower

dot :: Parser Char
dot = satisfy $ \x -> x == '.'

lambda :: Parser Char
lambda = char '\\'

application :: Parser Expr
application = chainl1 primary $ binaryOperator " " App

primary :: Parser Expr
primary = char '(' *> abstraction <* char ')' -- No explicit grouping created here, still works
    <|> EBool <$> (string "false" $> False)
    <|> EBool <$> (string "true" $> True)
    <|> Var <$> variable
    <|> Metavariable <$> metavariable
    <|> (lambdaBinding >>= abstraction') -- Allows for expressions like \x.x \x.x -> \x.(x (\x.x))






