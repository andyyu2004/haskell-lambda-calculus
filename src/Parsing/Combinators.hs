{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -w #-}

module Parsing.Combinators
    ( test,
      Parser,
      chainl1,
      symbol,
      doubleStr,
      spaceConsumer,
      binaryOperator
    ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Functor

-- ParsecT e s m a
-- Parsec = ParsecT e s Identity a (non-transformer)
-- e is custom component of error messages
-- s is the type of input stream

type Parser = Parsec Void T.Text

test :: IO ()
test = putStrLn "someFunc"

digit :: Parser Char
digit = satisfy (\c -> '0' <= c && c <= '9')

binaryOperator :: T.Text -> (a -> a -> a) -> Parser (a -> a -> a)
binaryOperator opsymbol op = string opsymbol $> op

number :: Parser String
number = many digit

posNum :: Parser String
posNum = some digitChar

dotNum :: Parser String
dotNum = (:) <$> char '.' <*> posNum

doubleStr :: Parser String
doubleStr =  (++) <$> posNum <*> (dotNum <|> return "") <* space

run :: Parser a -> T.Text -> Either (ParseErrorBundle T.Text Void) a
run p = parse p ""

numDigit :: Parser Double
numDigit = read <$> number

spaceConsumer :: Parser ()
spaceConsumer = L.space
              space1
              (L.skipLineComment "//")
              (L.skipBlockComment "/*" "*/")

symbol :: T.Text -> Parser T.Text
symbol = L.symbol spaceConsumer

-- Parse parser and continually parse operation iteratively while possible
-- <expr> ::= number { + number }
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = p >>= rest
  where rest a = (op >>= \f ->
                 p >>= \b ->
                 rest (f a b))
                 <|> return a

