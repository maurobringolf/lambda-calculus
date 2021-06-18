module Interpreter where

import Data.Set
import Ast
import Parser

lazyEvalInterp :: [Variable] -> Term -> Term
lazyEvalInterp fs (App t1 t2) = case lazyEvalInterp fs t1 of
  (Abs x t1') -> lazyEvalInterp fs (subst x t1' t2)
  (Var f) -> if f `elem` fs then App (Var f) (lazyEvalInterp fs t2)
             else App t1 t2
  t1'         -> App t1' t2
lazyEvalInterp fs t = t

lazyEval :: Term -> Term
lazyEval = lazyEvalInterp []

eagerEval :: Term -> Term
eagerEval (App t1 t2) = let t2' = eagerEval t2 in case eagerEval t1 of
  (Abs x t1') -> eagerEval (subst x t1' t2')
  t1'         -> App t1' t2'
eagerEval (Abs x t) = Abs x (eagerEval t)
eagerEval v = v

data EvaluationStrategy = Lazy | Eager

eval :: EvaluationStrategy -> Term -> Term
eval s = case s of
  Eager -> eagerEval
  Lazy -> lazyEval
