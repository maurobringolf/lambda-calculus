module SyntaxSugar.Compiler where

import Data.List(find)
import qualified Data.Set

import qualified SyntaxSugar.Ast as SS
import Ast
import Parser
import qualified Interpreter

eval :: RepresentableLC a => SS.Exp -> a
eval = evalRep . compile

exec :: RepresentableLC a => SS.Program -> a
exec = evalRep . compileMain

compileMain :: SS.Program -> Term
compileMain = compile . buildMain

buildMain :: SS.Program -> SS.Exp
buildMain (SS.P defs) = let ds = map desugarRecursion defs
                            main = case find ((== "main") . fst) ds of
                                     Just ("main", e) -> e
                                     Nothing -> error "No main function given."
                         in
                            foldr (\(f,e) m -> SS.App (SS.Abs f m) e) main (filter ((/= "main") . fst) ds)

desugarRecursion :: SS.Definition -> (String, SS.Exp)
desugarRecursion (SS.Def f e) = (f, if f `Data.Set.notMember` SS.freeVars e then e else SS.App y (SS.Abs f e))

y :: SS.Exp
y = SS.Abs "f" (SS.App (SS.Abs "x" (SS.App (SS.Var "f") (SS.App (SS.Var "x") (SS.Var "x")))) (SS.Abs "x" (SS.App (SS.Var "f") (SS.App (SS.Var "x") (SS.Var "x")))))

compile :: SS.Exp -> Term
compile e = case e of
  (SS.Var x) -> Var x
  (SS.Abs x e) -> Abs x $ compile e
  (SS.App e1 e2) -> App (compile e1) (compile e2)

  (SS.Numeral n) -> repLC n
  SS.Add -> churchAdd
  SS.Sub -> churchMinus
  SS.Mult -> churchMult
  SS.Eq -> churchEq
  SS.Leq -> churchLeq
  SS.Geq -> churchGeq
  SS.And -> churchAnd
  SS.Or -> churchOr

  (SS.Boolean b) -> repLC b
  (SS.IfElse b e1 e2) -> App (App (compile b) (compile e1)) (compile e2)

  e -> error $ show e

churchAdd :: Term
churchAdd = parse "(λm.λn.λS.λZ.m S (n S Z))"

churchMult :: Term
churchMult = parse "(λm.λn.λS.λZ.m (n S) Z)"

churchPred :: Term
churchPred = parse churchPred'

churchPred' :: String
churchPred' = "(λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u))"

churchMinus :: Term
churchMinus = parse churchMinus'

churchMinus' :: String
churchMinus' = "(λm.λn. n " ++ churchPred' ++ " m)"

churchEq :: Term
churchEq = parse churchEq'

churchEq' :: String
churchEq' = "(λm.λn." ++ churchAnd' ++ " (" ++ churchLeq' ++" m n) (" ++ churchLeq' ++" n m))"

churchLeq :: Term
churchLeq = parse churchLeq'

churchLeq' :: String
churchLeq' = "(λm.λn. " ++ churchIsZero' ++ " (" ++ churchMinus' ++ " m n))"

churchGeq :: Term
churchGeq = parse churchGeq'

churchGeq' :: String
churchGeq' = "(λn.λm. (" ++ churchIsZero' ++ ") (" ++ churchMinus' ++ " m n))"

churchIsZero :: Term
churchIsZero = parse churchIsZero'

churchIsZero' :: String
churchIsZero' = "(λn.n (λx." ++ churchBool' False ++ ") " ++ churchBool' True ++ ")"

churchAnd :: Term
churchAnd = parse churchAnd'

churchAnd' :: String
churchAnd' = "(λp.λq.p q p)"

churchOr :: Term
churchOr = parse churchOr'

churchOr' :: String
churchOr' = "(λp.λq.p p q)"

churchBool :: Bool -> Term
churchBool = parse . churchBool'

churchBool' :: Bool -> String
churchBool' True = "(λa.λb.a)"
churchBool' False = "(λa.λb.b)"

class RepresentableLC a where
  repLC :: a -> Term
  absLC :: Term -> a
  evalRep :: Term -> a

instance RepresentableLC Integer where
  repLC n = Abs "S" (Abs "Z" (foldr (\_ m -> (App (Var "S")) m) (Var "Z") [1..n]))
  evalRep = absLC . Interpreter.eval (Interpreter.LazyWithInterpretedSymbols ["S", "Z"]) . (\t -> App (App t (Var "S")) (Var "Z"))

  absLC (Var "Z") = 0
  absLC (App (Var "S") t) = 1 + absLC t
  absLC x = error $ "not a church numeral: " ++ show x

instance RepresentableLC Bool where
  repLC b = churchBool b
  absLC (Var "True") = True
  absLC (Var "False") = False
  absLC x = error $ "not a church boolean: " ++ show x
  evalRep = absLC . Interpreter.eval (Interpreter.LazyWithInterpretedSymbols ["True", "False"]) . (\t -> App (App t (Var "True")) (Var "False"))
