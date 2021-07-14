{-# LANGUAGE TemplateHaskell #-}

module ChurchEncoding.Compiler where

import Data.FileEmbed
import Data.ByteString.Char8(unpack)

import Data.List(find, intercalate)
import qualified Data.Set

import qualified ChurchEncoding.Ast as SS
import qualified ChurchEncoding.Parser
import ChurchEncoding.TypeChecker.TypeChecker(getMainType, inferConsType)
import ChurchEncoding.TypeChecker.Type(Type(..), getArgs)
import Ast
import Parser
import qualified Interpreter


execTyped :: SS.Program -> String
execTyped p = let p' = appendProgram stdLib p
                  t = getMainType p'
                  res = Interpreter.lazyEval $ compileMain p
               in
                 absT t res

absT :: Type -> Term -> String
absT TInt te = show $ (absLC::Term -> Integer) $ te
absT TBool te = show $ (absLC::Term -> Bool) $ te
absT (TList t) te = "[" ++ intercalate "," (map (absT t) . (absLC::Term -> [Term]) $ te) ++ "]"
absT (TVar _) te = show te

eval :: RepresentableLC a => SS.Exp -> a
eval = absLC . Interpreter.lazyEval . compile

exec :: RepresentableLC a => SS.Program -> a
exec = absLC . Interpreter.lazyEval . compileMain

compileMain :: SS.Program -> Term
compileMain p = let (SS.P funDefs dataDefs) = appendProgram stdLib p
                    conss = concatMap compileConstructors dataDefs
                 in
                  foldr (\(consName, consExp) e -> App (Abs consName e) consExp) (compile (buildMain' funDefs)) conss

compileConstructors :: SS.DataDef -> [(String, Term)]
compileConstructors (SS.DDef adt conss) = map (\cons -> let consT = inferConsType adt cons
                                                            numArgs = length $ getArgs consT
                                                            args = map (\n -> "x" ++ show n) [1..numArgs]
                                                         in
                                                           (SS.getConsName cons, foldr (\c -> Abs c) (foldl App (Var $ SS.getConsName cons) (map Var args)) (args ++ (map SS.getConsName conss)))) conss

stdLib :: SS.Program
stdLib = ChurchEncoding.Parser.parse $ unpack $(embedFile "lib/stdlib.hs")

appendProgram :: SS.Program -> SS.Program -> SS.Program
appendProgram (SS.P f1 d1) (SS.P f2 d2) = SS.P (f1 ++ f2) (d1 ++ d2)

-- TODO unify those two
buildMain' :: [SS.FunDef] -> SS.Exp
buildMain' fs = buildMain (SS.P fs [])

buildMain :: SS.Program -> SS.Exp
buildMain (SS.P defs dataDefs) = let ds = map desugarRecursion defs
                                     main = case find ((== "main") . fst) ds of
                                              Just ("main", e) -> e
                                              Nothing -> error "No main function given."
                                  in
                                     foldr (\(f,e) m -> SS.App (SS.Abs f m) e) main (filter ((/= "main") . fst) ds)

desugarRecursion :: SS.FunDef -> (String, SS.Exp)
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

  (SS.List xs) -> repLC $ map compile xs
  SS.Cons -> churchCons
  SS.Foldr -> Abs "f" $ Abs "b" $ Abs "xs" $ (App (App (Var "xs") (Var "f")) (Var "b"))
  SS.Tail -> parse "(λl.λc.λn.l (λh.λt.λg.g h (t c)) (λt.n) (λh.λt.t))"

  (SS.Constructor cons) -> Var cons
  (SS.CaseOf e ps) -> let te = compile e
                          tps = map (\(p,e) -> foldr Abs (compile e) (SS.getPatternVars p)) ps
                       in
                        foldl App te tps

churchCons :: Term
churchCons = parse churchCons'

churchCons' :: String
churchCons' = "(λh.λt.λc.λn.c h (t c n))"

churchAdd :: Term
churchAdd = parse churchAdd'

churchAdd' :: String
churchAdd' = "(λm.λn.λS.λZ.m S (n S Z))"

churchMult :: Term
churchMult = parse ("(λm.λn.m (" ++ churchAdd' ++ " n) " ++ show (repLC (0::Integer)) ++ ")")

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

instance RepresentableLC Integer where
  repLC n = Abs "S" (Abs "Z" (foldr (\_ m -> (App (Var "S")) m) (Var "Z") [1..n]))
  absLC = convert . Interpreter.eval (Interpreter.LazyWithInterpretedSymbols ["S", "Z"]) . (\t -> (App (App t (Var "S")) (Var "Z")))
      where
        convert (Var "Z") = 0
        convert (App (Var "S") t) = 1 + convert t
        convert x = error $ "not a church numeral: " ++ show x

instance RepresentableLC Bool where
  repLC b = churchBool b
  absLC = convert . Interpreter.eval (Interpreter.LazyWithInterpretedSymbols ["True", "False"]) . (\t -> App (App t (Var "True")) (Var "False"))
      where
        convert (Var "True") = True
        convert (Var "False") = False
        convert x = error $ "not a church boolean: " ++ show x

instance RepresentableLC Term where
  repLC = id
  absLC = id

instance (RepresentableLC a) => RepresentableLC [a] where
  repLC xs = Abs "c" (Abs "n" (foldr (\x a -> App (App (Var "c") (repLC x)) a) (Var "n") xs))
  absLC = convert . Interpreter.eval (Interpreter.LazyWithInterpretedSymbols ["Cons", "Nil"]) . (\t -> App (App t (Var "Cons")) (Var "Nil"))
    where
      convert ts = case ts of
        (Var "Nil") -> []
        (App (App (Var "Cons") h) t) -> absLC h : convert (Interpreter.lazyEval t)
        e -> error $ "Not a church list: " ++ show e
