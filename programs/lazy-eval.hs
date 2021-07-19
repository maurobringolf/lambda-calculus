data Exp = Var Int | App Exp Exp | Abs Int Exp

delete x xs = if length xs == 0 then [] else
  (if head xs == x then tail xs else head xs : (delete x (tail xs)))

delete x = filter (\y -> not (y == x))

elem y xs = foldr (\x t -> t || x == y) False xs

fresh xs = 1 + (foldr (\x c -> if x >= c then x else c) 0 xs)

freeVars e = case e of
  Var x -> [x];
  App t1 t2 -> concat (freeVars t1) (freeVars t2);
  Abs x t -> delete x (freeVars t);


subst x e t = case e of
  (Var y) -> if x == y then t else (Var y);
  (App e1 e2) -> App (subst x e1 t) (subst x e2 t);
  (Abs y e2) -> if x == y then
                  (Abs y e2)
                else if elem y (freeVars t) then
                  Abs (fresh (concat (freeVars e2) (freeVars t))) (subst x (subst y e2 (Var (fresh (concat (freeVars e2) (freeVars t))))) t)
                else
                  (Abs y (subst x e2 t));

lazyEval e = case e of
  App e1 e2 -> (case lazyEval e1 of
    Abs x e -> lazyEval (subst x e e2);
    App e1a e1b -> App e1 e2;
    Var x -> App e1 e2;
  );
  Var x -> Var x;
  Abs x e -> Abs x e;

main = lazyEval
