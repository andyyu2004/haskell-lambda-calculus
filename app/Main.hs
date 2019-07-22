{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parsing.Combinators
import Parsing.LambdaExpressions
import Parsing.PrettyPrinting
import Text.Megaparsec
import Data.Text as T
import System.Console.Haskeline


main :: IO ()
main = runInputT defaultSettings loop

loop :: InputT IO ()
loop = do
    line <- getInputLine "\\>> "
    case line of
        Nothing -> return ()
        Just "" -> loop
        Just x -> evaluateInput x >> loop

evaluateInput :: String -> InputT IO ()
evaluateInput input = do
    let text = T.pack input
    case parse expression "lambda_calculus" text of
        Left  e -> outputStrLn (errorBundlePretty e)
        Right x -> (outputStrLn . show) x >>
          (outputStrLn . parenthesized) x >>
          (outputStrLn . formatExpr) x