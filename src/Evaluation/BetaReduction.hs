module Evaluation.BetaReduction
    (
      betareduce,
--      evaluate,
--      expandBindings,
      evaluate'
    ) where

import Parsing.LambdaExpressions(Expr(..))
import Evaluation.Substitution
import Data.Map.Lazy as M
import Text.Printf
--import Control.Monad.State

-- (\x.E) N -> E [x -> N]
betareduce :: Expr -> Expr
betareduce (Abstraction name expr) = Abstraction name $ betareduce expr
betareduce var@(Variable _) = var
betareduce (Binding _ expr) = betareduce expr
betareduce (Application left right) =
  -- Need to betareduce left here otherwise will get stuck in beta reduce abstraction infinite loop
  case betareduce left of
    Abstraction name expr -> betareduce $ substitute (betareduce right) name $ betareduce expr
    _ -> Application (betareduce left) $ betareduce right
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

evaluate' :: M.Map String Expr -> Expr -> (Either String Expr, M.Map String Expr)
evaluate' varmap expression = let (expr, varmap') = expandBindings' varmap expression in
    (betareduce <$> expr, varmap')

expandBindings' :: M.Map String Expr -> Expr -> (Either String Expr, M.Map String Expr)
expandBindings' varmap (Binding name expression) =
  let (x, _) = expandBindings' varmap expression
    in case x of
        Left _ -> (x, varmap)
        Right r -> (x, M.insert name r varmap)
expandBindings' varmap (Metavariable name) = case M.lookup name varmap of
    Just x -> (Right x, varmap)
    Nothing -> (Left $ printf "Undefined metavariable %s" name, varmap)
expandBindings' varmap var@(Variable _) = (Right var, varmap)
expandBindings' varmap (Abstraction name expr) = (Abstraction name <$> fst (expandBindings' varmap expr), varmap)
expandBindings' varmap (Application left right) =
    (Application <$> fst (expandBindings' varmap left) <*> fst (expandBindings' varmap right), varmap)


