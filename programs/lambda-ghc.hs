data Exp = Var Int | App Exp Exp | Abs Int Exp
  deriving Show

delete x = filter (\y -> not (y == x))

fresh xs = 1 + (foldr (\x c -> if x >= c then x else c) 0 xs)

freeVars e = case e of
  Var x -> [x]
  App t1 t2 -> (++) (freeVars t1) (freeVars t2)
  Abs x t -> delete x (freeVars t)

subst x e t = case e of
  (Var y) -> if x == y then t else (Var y)
  (App e1 e2) -> App (subst x e1 t) (subst x e2 t)
  (Abs y e2) -> if x == y then
                  (Abs y e2)
                else if elem y (freeVars t) then
                  Abs (fresh ((++) (freeVars e2) (freeVars t))) (subst x (subst y e2 (Var (fresh ((++) (freeVars e2) (freeVars t))))) t)
                else
                  (Abs y (subst x e2 t))

lazyEval fs e = case e of
  App e1 e2 -> (case lazyEval fs e1 of
    Abs x e -> lazyEval fs (subst x e e2)
    App e1a e1b -> App e1 e2
    Var x -> if elem x fs then App e1 (lazyEval fs e2) else App e1 e2
    )
  Var x -> Var x
  Abs x e -> Abs x e

_main = (\res -> lazyEval [0,1] (App (App res (Var 1)) (Var 0))) (lazyEval []
  (App
      (App
        (Abs 2 (Abs 3 (Abs 1 (Abs 0 (App (App (Var 2) (Var 1)) (App (App (Var 3) (Var 1)) (Var 0)))))))
        (Abs 1 (Abs 0 (App (Var 1) (Var 0)))))
      (Abs 1 (Abs 0 (App (Var 1) (App (Var 1) (Var 0)))))
    )
  )
