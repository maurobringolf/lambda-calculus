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
  show (Var x) = x
  show (Abs x t) = "λ" ++ x ++ "." ++ show t
  show (App t1 t2) = showAppL t1 ++ " " ++ showAppR t2
    where
      showAppL (Abs x t) = "(" ++ show (Abs x t) ++ ")"
      showAppL t = show t
      showAppR (App t s) = "(" ++ show (App t s) ++ ")"
      showAppR t = show t

subst :: Variable -> Term -> Term -> Term
subst x (Var y) t
  | x == y = t
  | x /= y = Var y
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

