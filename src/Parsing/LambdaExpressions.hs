module Parsing.Expression
  (

  ) where
    
import Parsing.Combinators
import Text.Megaparsec
import Text.Megaparsec.Char

data Expr =
  Variable Char
  | Abstraction Char Expr
  | Application Expr Expr
  | Grouping Expr

{-
  <expr> ::= <abstraction>
  <abstraction> ::= \<var>.<abstraction> | <application>
  <application> = <primary>  { < > <primary> }
  <primary> ::= <var> | ( <expr> )
  <var> ::= ID
-}

expr :: Parser Expr
expr = abstraction

abstraction :: Parser Expr
abstraction = () <|> application

application :: Parser Expr
application = primary `chainl1` 

