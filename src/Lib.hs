{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Lib
    ( test
    ) where

import Text.Megaparsec
import qualified Data.Text as T
import Data.Void

import Parsing.Combinators
import Parsing.Arithmetic


--type Parser = Parsec Void T.Text
--
--test :: IO ()
--test = putStrLn "someFunc"
--
--digit :: Parser Char
--digit = satisfy (\c -> '0' <= c && c <= '9')
--
--number :: Parser String
--number = some digit

{-
  <expr> ::= <expr> + number
  ->
  <expr> ::= number { + number }

  parse_expr :: expr -> (expr -> expr -> expr) -> expr
  parse_expr expr = expr + parse_expr

-}

-- Parse parser and continually parse operation iteratively while possible
-- <expr> ::= number { + number }
--chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
--p `chainl1` op = p >>= rest
--  where rest a = (op >>= \f ->
--                 p >>= \b ->
--                 rest (f a b))
--                 <|> return a





