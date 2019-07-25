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
    primary
  ) where
    
import Parsing.Combinators
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Char
import Text.Printf

data Expr =
    Variable Char
    | Abstraction Char Expr
    | Application Expr Expr
    | Binding String Expr
    | Metavariable String
--    | Grouping Expr (Unnecessary?)
    deriving Show

{-
  <expr> ::= <abstraction>
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
expression = letBinding <* eof

-- Metavariable binding

-- Extra function so that let X = Y = E is valid instead of let X = let Y = E
letBinding :: Parser Expr
letBinding = ("let" >> spaceConsumer >> binding)
             <|> abstraction

binding :: Parser Expr
binding = (Binding <$> metavariable <* spaceConsumer <*> (char '=' >> spaceConsumer >> abstraction))-- binding <|> abstraction))
          <|> abstraction

abstraction :: Parser Expr
abstraction = lambdaBinding <*> abstraction <|> application

-- For primary parsing where the binding is already parsed. Otherwise infinite loop if abstraction is called directly
abstraction' :: (Expr -> Expr) -> Parser Expr
abstraction' var = var <$> abstraction <|> application

-- Parses the lambda and the variable, partially applies the Abstraction constructor
lambdaBinding :: Parser (Expr -> Expr)
lambdaBinding = Abstraction <$> (lambda *> variable <* dot)

variable :: Parser Char
variable = satisfy isAsciiLower <?> printf "variable name in %s" (show varNames)

dot :: Parser Char
dot = satisfy $ \x -> x == '.'

lambda :: Parser Char
lambda = char '\\'

application :: Parser Expr
application = chainl1 primary $ binaryOperator " " Application

primary :: Parser Expr
primary = Variable <$> variable
    <|> char '(' *> abstraction <* char ')' -- No explicit grouping created here, still works
    <|> Metavariable <$> metavariable
    <|> (lambdaBinding >>= abstraction') -- Allows for expressions like \x.x \x.x -> \x.(x (\x.x))






