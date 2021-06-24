module SyntaxSugar.Compiler where

import Data.List(find)

import qualified SyntaxSugar.Ast as SS
import Ast
import Parser
import qualified Interpreter

eval :: RepresentableLC a => SS.Exp -> a
eval = evalRep . compile

exec :: RepresentableLC a => SS.Program -> a
exec = evalRep . compile . buildMain

buildMain :: SS.Program -> SS.Exp
buildMain (SS.P defs) = let ds = map desugarRecursion defs
                            main = case find ((== "main") . fst) ds of
                                     Just ("main", e) -> e
                                     Nothing -> error "No main function given."
                         in
                            foldr (\(f,e) m -> SS.App (SS.Abs f m) e) main (filter ((/= "main") . fst) ds)

desugarRecursion :: SS.Definition -> (String, SS.Exp)
desugarRecursion (SS.Def f e) = (f, SS.App y (SS.Abs f e))

y = SS.Abs "f" (SS.App (SS.Abs "x" (SS.App (SS.Var "f") (SS.App (SS.Var "x") (SS.Var "x")))) (SS.Abs "x" (SS.App (SS.Var "f") (SS.App (SS.Var "x") (SS.Var "x")))))

compile :: SS.Exp -> Term
compile e = case e of
  (SS.Var x) -> Var x
  (SS.Abs x e) -> Abs x $ compile e
  (SS.App e1 e2) -> App (compile e1) (compile e2)
  (SS.Numeral n) -> repLC n
  SS.Add -> churchAdd
  SS.Mult -> churchMult

churchAdd :: Term
churchAdd = parse "λm.λn.λS.λZ.m S (n S Z)"

churchMult :: Term
churchMult = parse "λm.λn.λS.λZ.m (n S) Z"

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

