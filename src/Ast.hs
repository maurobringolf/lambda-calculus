module Ast where

import Data.Set

type Variable = String

data Term = Var Variable | Abs Variable Term | App Term Term

instance Eq Term where
  (==) (Var x) (Var y) = x == y
  (==) (App t1 t2) (App s1 s2) = (t1 == s1) && (t2 == s2)
  (==) (Abs x t) (Abs y s) = let z = fresh (freeVars t `union` freeVars s) in
    (subst x t (Var z)) == (subst y s (Var z))
  (==) _ _ = False

instance Show Term where
  show = showAbs
    where showAtom (Var x) = x
          showAtom t = "(" ++ show t ++ ")"

          showAbs (Abs x t) = "λ" ++ x ++ "." ++ showAbs t
          showAbs t = showApp t

          showApp (App t1 t2) = showApp t1 ++ " " ++ showAtom t2
          showApp t = showAtom t
  


subst :: Variable -> Term -> Term -> Term
subst x (Var y) t = case x == y of
  True -> t
  False -> Var y
subst x (App s1 s2) t = App (subst x s1 t) (subst x s2 t)
subst x (Abs y s) t
  | x == y = Abs y s
  | y `notMember` freeVars t = Abs y (subst x s t)
  | otherwise = let z = fresh (freeVars t `union` freeVars s) in
    Abs z (subst x (subst y s (Var z)) t)

freeVars :: Term -> Set Variable
freeVars (Var x) = singleton x
freeVars (App t1 t2) = freeVars t1 `union` freeVars t2
freeVars (Abs x t) = delete x (freeVars t)

fresh :: Set Variable -> Variable
fresh xs = if Data.Set.null xs then "x" else findMax xs ++ "0"

getHeadSymbol :: Term -> Variable
getHeadSymbol t = case t of
  Var z -> z
  (App t1 _) -> getHeadSymbol t1
  (Abs _ _) -> error "getHeadSymbol of (Abs _ _)"

getArgTerms :: Term -> [Term]
getArgTerms (App (Var _) t2) = [t2]
getArgTerms (App t1 t2) = getArgTerms t1 ++ [t2]
getArgTerms t = [t]
