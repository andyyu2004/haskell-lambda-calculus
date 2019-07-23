{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parsing.Combinators
import Parsing.LambdaExpressions
import Parsing.PrettyPrinting
import Text.Megaparsec (parse, errorBundlePretty)
import Evaluation.LambdaCombinators
import qualified Data.Text as T
import System.Console.Haskeline
import Text.Printf
import Evaluation.AlphaConversion
import Evaluation.Substitution
import Evaluation.BetaReduction
import qualified Data.Map.Lazy as M -- No hashmap in haskell?
import Control.Monad.State

main :: IO ()
main = runInputT defaultSettings $ loop $ M.fromList lambdaCombinators

loop :: VariableMap -> InputT IO ()
loop vmap = do
  line <- getInputLine "\\>> "
  case line of
      Nothing -> return ()
      Just "" -> loop vmap
      Just (x:xs) | x == ':' -> handleCommand vmap xs >> loop vmap
      Just input -> do
        let desugared = desugarAbstraction input
        outputStrLn $ printf "desugared: %s" desugared
        case parse expression "lambda_calculus" (T.pack desugared) of
          Left e -> outputStrLn (errorBundlePretty e) >> loop vmap
          Right x -> do
            (outputStrLn . ("raw: " ++) . show) x
            outputStrLn ("parenthesized: " ++ parenthesized x)
            let (expr, state) = runState (evaluate x) vmap
            case expr of
              Left err -> outputStrLn err >> loop state
              Right x-> do
                (outputStrLn . ("β-reduction (raw): " ++) .  show) x
                (outputStrLn . ("β-reduction (parenthesized): " ++) . parenthesized) x
                (outputStrLn . ("β-reduction (spaces): " ++) . formatExpr) x
                (outputStrLn . ("β-reduction: " ++) . nospaces) x
                outputStr "\n"
                loop state


handleCommand :: VariableMap -> String -> InputT IO ()
handleCommand vmap cmd
  | cmd == "h" || cmd == "help" = printHelpText
  | cmd == "e" || cmd == "env" = outputStrLn . ("env:\n" ++) . formatList $ formatVMap vmap
  | otherwise = outputStrLn $ "unknown command: " ++ cmd

formatList :: [(String, String)] -> String
formatList xs = foldl1 (\acc x -> acc ++ '\n' : x) $ map (uncurry (printf "%s <- %s")) xs -- map (\(name, expr) -> printf "%s <- %s" name expr) xs

formatVMap :: VariableMap -> [(String, String)]
formatVMap vmap = map (\(name, x) -> (name, formatExpr x)) $ M.toList vmap



-- Without using state
--main :: IO ()
--main = runInputT defaultSettings $ loop' $ M.fromList lambdaCombinators

--loop' :: M.Map String Expr -> InputT IO ()
--loop' vmap = do
--    line <- getInputLine "\\>> "
--    case line of
--        Nothing -> return ()
--        Just "" -> loop' vmap
--        Just input -> do
--          let desugared = desugarAbstraction input
--          outputStrLn $ printf "desugared: %s" desugared
--          case parse expression "lambda_calculus" (T.pack desugared) of
--            Left e -> outputStrLn (errorBundlePretty e) >> loop' vmap
--            Right x -> do
--                (outputStrLn . ("raw: " ++) . show) x
--                outputStrLn ("parenthesized: " ++ parenthesized x)
--                (outputStrLn . ("Standard notation: " ++) . formatExpr) x
----                (outputStrLn . ("β-reduction (parenthesized): " ++) . formatError formatExpr . evaluate varmap) x
----                (outputStrLn . ("β-reduction: " ++) . formatError formatExpr . evaluate varmap) x
--                let (reduction, newmap) = evaluate' vmap x
--                (outputStrLn . ("β-reduction (raw): " ++) . formatError show) reduction
--                (outputStrLn . ("β-reduction (parenthesized): " ++) . formatError parenthesized) reduction
--                (outputStrLn . ("β-reduction: " ++) . formatError formatExpr) reduction
--                outputStr "\n"
--                loop' newmap


--formatError :: (Show a) => (Expr -> String) -> Either a Expr -> String
--formatError formatter (Right expr) = formatter expr
--formatError _ (Left err) = show err ++ "\n :help"

printHelpText :: InputT IO ()
printHelpText = do
    outputStrLn "Use backslash '\\' as lambda"
    outputStrLn "Allows syntactic sugar for multiple abstractions: \\xyz.x y z -> \\x.\\y.\\z.x y z"
    outputStrLn "Lambda variables currently must be a single lower case character, though this may change"
    outputStrLn "Explicit spaces are required for application"
    outputStrLn "Application has higher precedence than abstraction, standard associativity rules apply"
    outputStrLn "Applying abstractions without parentheses is allowed"
    outputStrLn "i.e. \\x.x \\y.y -> \\x.(x (\\y.y))"
    outputStrLn "You are allowed to set bindings to lambda expressions using 'let <metavariable> = <expression>' syntax"
    outputStrLn "Metavariables begin with a capital letter"
    outputStrLn "Examples: FALSE = \\xy.y, M = \\f.f f, Foo = x"
    outputStrLn "Some names are by default bound to combinators, including {{ I, K, KI, B, T, M }} and boolean operators {{ NOT, AND, OR }}"
    outputStrLn "Use :e or :env for see current bindings"

