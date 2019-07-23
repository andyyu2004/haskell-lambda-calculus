module Evaluation.LambdaCombinators
    (
        lambdaCombinators,
    ) where

import Parsing.LambdaExpressions
import Text.Megaparsec(parse)
import Evaluation.BetaReduction(evaluate)
import Data.Text as T
import Control.Monad.State
import Data.Map.Lazy(fromList)

lambdaCombinators :: [(String, Expr)]
lambdaCombinators =
  [
    ("I", identity),
    ("M", mockingbird),
    ("K", kestrel),
    ("KI", kite),
    ("B", bluebird),
    ("T", thrush),
    ("V", vireo),

    ("0", zero),
    ("1", one),
    ("2", two),
    ("3", three),
    ("4", four),
    ("SUCC", successor),
    ("ISZERO", iszero),

    ("TRUE", true),
    ("FALSE", false),
    ("AND", andCombinator),
    ("OR", orCombinator),
    ("NOT", notCombinator),
    ("BEQ", beq),
    ("XOR", xor)
  ]

identity :: Expr
identity = forceParse "\\x.x"

mockingbird :: Expr
mockingbird = forceParse "\\f.f f"

kestrel :: Expr
kestrel = forceParse "\\xy.x"

kite :: Expr
kite = forceParse "\\xy.y"

bluebird :: Expr
bluebird = forceParse "\\fga.f (g a)"

thrush :: Expr
thrush = forceParse "\\ab.b a"

-- Partially apply two values and then pass a binary function to use value
-- A form of data structure
vireo :: Expr
vireo = forceParse "\\abf.f a b"

false :: Expr
false = forceEvaluate "KI"

true :: Expr
true = forceEvaluate "K"

-- Boolean equality
beq :: Expr
beq = forceEvaluate "\\pq.p q (NOT q)"

xor :: Expr
xor = forceEvaluate "\\pq.p (NOT q) q"

orCombinator :: Expr
--orCombinator = forceParse "\\pq.p p q"
orCombinator = forceEvaluate "M"

andCombinator :: Expr
andCombinator = forceParse "\\pq.p q p"

notCombinator :: Expr
notCombinator = forceEvaluate "\\b.b KI K"

zero :: Expr
zero = forceParse "\\fx.x"

one :: Expr
one = forceEvaluate "SUCC 0"

two :: Expr
two = forceEvaluate "1 SUCC 1"

three :: Expr
three = forceEvaluate "2 SUCC 1"

four :: Expr
four = forceEvaluate "T 2 2" -- (T is exp, B is mult)

iszero :: Expr
iszero = forceEvaluate "\\n.n (K FALSE) K" -- (Must apply constant false function 0 times to be true)

successor :: Expr
successor = forceEvaluate "\\nf.B f (n f)"
--successor = forceParse "\\nfx.f (n f x)" -- -> \\nfx.B f (n f) x (let g = n f)

unwrap :: Either a b -> b
unwrap (Left _)  = error "Found left"
unwrap (Right x) = x

forceParse :: String -> Expr
forceParse = unwrap . parse expression "lambdacalculus" . T.pack . desugarAbstraction

-- Use if metavariables are part of the expression
-- String -> Expr ->
forceEvaluate :: String -> Expr
forceEvaluate expr = unwrap $ evalState (evaluate $ forceParse expr) (fromList lambdaCombinators)

--forceEvaluate :: String -> Expr
--forceEvaluate = unwrap . fst . evaluate' (fromList lambdaCombinators) . forceParse



