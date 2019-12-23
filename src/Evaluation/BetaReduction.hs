{-# LANGUAGE LambdaCase #-}
module Evaluation.BetaReduction
    (
      betareduce,
      evaluate,
      VariableMap,
--      expandBindings,
--      evaluate'
    ) where

import Parsing.LambdaExpressions(Expr(..))
import Evaluation.Substitution
import Data.Map.Lazy as M
import Text.Printf
import Control.Monad.State
--import Debug.Trace

-- (\x.E) N -> E [x -> N]
betareduce :: Expr -> Expr
betareduce (Lambda name expr) = Lambda name $ betareduce expr
betareduce var@(Var _) = var
betareduce (Binding _ expr) = betareduce expr
betareduce (App left right) =
  -- Need to betareduce left here otherwise will get stuck in beta reduce abstraction infinite loop
  case betareduce left of
    Lambda name expr -> betareduce $ substitute (betareduce right) name $ betareduce expr
    _ -> App (betareduce left) $ betareduce right
betareduce (Metavariable _) = error "Betareduction of metavariable"

-- fmap :: (a -> b) -> m a -> m b
--evaluate :: M.Map String Expr -> Expr -> Either String Expr
--evaluate varmap expr = betareduce <$> expandBindings varmap expr
--
---- Use state monad to wrap around the explicit return of a pair
--expandBindings :: M.Map String Expr -> Expr ->  Either String Expr
--expandBindings varmap (Binding name expr) = Right (M.insert name expr varmap) >> Right expr
--expandBindings varmap (Metavariable name) = case M.lookup name varmap of
--    Just x -> Right x
--    Nothing -> Left $ printf "Undefined metavariable %s" name
--expandBindings _ expr = Right expr

type VariableMap = M.Map String Expr

evaluate :: Expr -> State VariableMap (Either String Expr)
evaluate expr = expandBindings expr >>= \x -> return $ betareduce <$> x

expandBindings :: Expr -> State VariableMap (Either String Expr)
expandBindings (Binding name expr) = insertVar name expr >>= expandBindings
expandBindings (Lambda name expr) = expandBindings expr >>= \x -> return $ fmap (Lambda name) x
expandBindings (App left right) = do
    l <- expandBindings left
    r <- expandBindings right
    return $ App <$> l <*> r
expandBindings (Metavariable name) = getVar name >>= \case
    Just x -> expandBindings x
    Nothing -> return $ Left $ printf "Undefined metavariable %s" name
expandBindings var = return $ Right var

-- Without error handling with either
---- (Expr -> m Expr) -> (Expr -> Expr) -> m Expr
--evaluate :: Expr -> State VariableMap Expr
--evaluate expr = betareduce <$> expandBindings expr
--
--expandBindings :: Expr -> State VariableMap Expr
--expandBindings (Binding name expr) = trace "bind" $ insertVar name expr >>= expandBindings
--expandBindings (Abstraction name expr) = Abstraction name <$> expandBindings expr
--expandBindings (Application left right) = Application <$> expandBindings left <*> expandBindings right
--expandBindings (Metavariable name) = getVar name >>= \case
--    Just x -> return x
--    Nothing -> error ""
--expandBindings var = return var

-- Inserts expr and return that expr
insertVar :: String -> Expr -> State VariableMap Expr
insertVar name expr = state $ \varmap -> (expr, M.insert name expr varmap)

---- Return Maybe Expr depending on the existence of the key
getVar :: String -> State VariableMap (Maybe Expr)
getVar name = state $ \varmap -> (M.lookup name varmap, varmap)

--pop :: State [a] a
--pop = state $ \(x:xs) -> (x, xs)
--
--push :: a -> State [a] ()
--push x = state $ \xs -> ((), x : xs)





-- Explicit handling of vmap
--evaluate' :: M.Map String Expr -> Expr -> (Either String Expr, M.Map String Expr)
--evaluate' varmap expression = let (expr, varmap') = expandBindings' varmap expression in
--    (betareduce <$> expr, varmap')
--
--expandBindings' :: M.Map String Expr -> Expr -> (Either String Expr, M.Map String Expr)
--expandBindings' varmap (Binding name expression) =
--  let (x, newmap) = expandBindings' varmap expression
--    in case x of
--        Left _ -> (x, newmap)
--        Right r -> (x, M.insert name r newmap)
--expandBindings' varmap (Metavariable name) = case M.lookup name varmap of
--    Just x -> (Right x, varmap)
--    Nothing -> (Left $ printf "Undefined metavariable %s" name, varmap)
--expandBindings' varmap var@(Variable _) = (Right var, varmap)
--expandBindings' varmap (Abstraction name expr) = (Abstraction name <$> fst (expandBindings' varmap expr), varmap)
--expandBindings' varmap (Application left right) =
--    (Application <$> fst (expandBindings' varmap left) <*> fst (expandBindings' varmap right), varmap)





