module SyntaxSugar.TypeChecker where

import SyntaxSugar.Ast
import Data.Map(Map)
import qualified Data.Map
import qualified Data.Set

type TypeContext = Map String Type

{-
freshTVar :: TypeContext -> Type
freshTVar ctx = let vars = [ x  | (_,TVar x) <- Data.Map.toList ctx ]
                 in
                  TVar $ foldr max 0 vars
-}

freshTVar :: TypeContext -> (Type, TypeContext)
freshTVar ctx = let vars = [ x  | (_,TVar x) <- Data.Map.toList ctx ]
                    fresh = 1 + (foldr max 0 vars)
                 in (TVar fresh, Data.Map.insert ("$$dummy" ++ show fresh) (TVar fresh) ctx)

--freshTVars :: TypeContext -> ([Type], TypeContext)
--freshTVars ctx = map TVar [(foldr max 0 [ x  | (_,TVar x) <- Data.Map.toList ctx ])..]
--freshTVars ctx = foldr (\_ (xs,c) -> let (x,c') = freshTVar c
 --                                     in (x:xs, c')) ([], ctx) [1..]


insertFreshTVar :: TypeContext -> String -> TypeContext
insertFreshTVar ctx x = let (ft,ctx') = freshTVar ctx
                         in
                          Data.Map.insert x ft ctx'

data Type = TInt
          | TBool
          | TFun Type Type
          | TVar Int
          | TList Type
  deriving (Eq, Ord)

equalsUpToRenaming :: Type -> Type -> Bool
equalsUpToRenaming t1 t2 = case equalsWithRenameLeft t1 t2 Data.Map.empty of
    Nothing -> False
    Just _ -> True

equalsWithRenameLeft :: Type -> Type -> Data.Map.Map Int Int -> Maybe (Data.Map.Map Int Int)
equalsWithRenameLeft t1 t2 r = case (t1,t2) of
  (TInt,TInt) -> Just r
  (TBool,TBool) -> Just r
  (TList t1, TList t1') -> equalsWithRenameLeft t1 t1' r
  (TFun t1 t2, TFun t1' t2') -> case equalsWithRenameLeft t1 t1' r of
    Nothing -> Nothing
    Just r' -> equalsWithRenameLeft t2 t2' r'
  (TVar i, TVar j) -> if i == j then
                        Just r
                      else case Data.Map.lookup i r of
                        Nothing -> Just $ Data.Map.insert i j r
                        Just j' -> if j == j' then Just r else Nothing
  (_,_) -> Nothing


instance Show Type where
  show TInt = "Int"
  show TBool = "Bool"
  show (TList t) = "[" ++ show t ++ "]"
  show (TFun f@(TFun t1 t2) t3) = "(" ++ show f ++ ") -> " ++ show t3
  show (TFun t1 t2) = show t1 ++ " -> " ++ show t2
  show (TVar i) = "τ" ++ show i

ftv :: Type -> Data.Set.Set Int
ftv (TVar i) = Data.Set.singleton i
ftv (TFun t1 t2) = Data.Set.union (ftv t1) (ftv t2)
ftv TInt = Data.Set.empty
ftv TBool = Data.Set.empty
ftv (TList t) = ftv t

data TypeError = TE Type Type
  deriving (Show, Eq)

getMainType :: Program -> Type
getMainType p = case Data.Map.lookup "main" (typeCheck p) of
                  Nothing -> error $ "Missing main function."
                  Just t -> t

typeCheck :: Program -> TypeContext
typeCheck (P defs) = foldl (\ctx (Def d e) -> let (ft, ctx') = freshTVar ctx
                                               in
                                                Data.Map.insert d (inferType (Data.Map.insert d ft ctx') e) ctx') (Data.Map.singleton "undefined" (TVar 0)) defs

inferType :: TypeContext -> Exp -> Type
inferType ctx e = let (te, ctx') = freshTVar ctx
                      eqs = buildEqs ctx' e te
                      s = unifyAll eqs
                   in case s of
                    Left s -> apply s te
                    Right r -> error $ show r

buildEqs :: TypeContext -> Exp -> Type -> Data.Set.Set TEQ
buildEqs ctx (Var x) t = case Data.Map.lookup x ctx of
  Nothing -> error $ "Variable " ++ x ++ " not in scope"
  Just t' -> if t' == t then Data.Set.empty else Data.Set.singleton $ TEQ t' t

buildEqs ctx (Abs x e) t = let (a1,ctx') = freshTVar ctx
                               (a2,ctx'') = freshTVar ctx'
                            in
                              (buildEqs (Data.Map.insert x a1 ctx'') e a2) `Data.Set.union` (Data.Set.singleton $ TEQ (TFun a1 a2) t)

buildEqs ctx (App t1 t2) t = let (a, ctx') = freshTVar ctx
                                 eqs1 = buildEqs ctx' t1 (TFun a t)
                                 vars = Data.Set.foldr (\(TEQ t1 t2) fv -> fv `Data.Set.union` (ftv t1 `Data.Set.union` ftv t2)) Data.Set.empty eqs1
                                 ctx'' = Data.Set.foldr (\x c -> (Data.Map.insert ("$$dummy" ++ show x) (TVar x) c)) ctx vars
                              in
                                eqs1 `Data.Set.union` buildEqs ctx'' t2 a

buildEqs ctx Add t = Data.Set.singleton $ TEQ t $ binaryOpType TInt
buildEqs ctx Sub t = Data.Set.singleton $ TEQ t $ binaryOpType TInt
buildEqs ctx Mult t = Data.Set.singleton $ TEQ t $ binaryOpType TInt
buildEqs ctx Foldr t = let (a1,ctx') = freshTVar ctx
                           (a2,_) = freshTVar ctx'
                        in
                          Data.Set.singleton $ TEQ t (TFun (TFun a1 (TFun a2 a2)) (TFun a2 (TFun (TList a1) a2)))

buildEqs _ (Numeral _) t = Data.Set.singleton $ TEQ TInt t
buildEqs _ (Boolean _) t = Data.Set.singleton $ TEQ TBool t
buildEqs ctx And t = Data.Set.singleton $ TEQ t $ binaryOpType TBool
buildEqs ctx Or t = Data.Set.singleton $ TEQ t $ binaryOpType TBool
buildEqs ctx Cons t = let (a,_) = freshTVar ctx in Data.Set.singleton $ TEQ t (TFun a (TFun (TList a) (TList a)))
buildEqs ctx (List xs) t = let (a, ctx') = freshTVar ctx
                           in
                             Data.Set.unions $ (Data.Set.singleton (TEQ t (TList a))) : map (\x -> buildEqs ctx' x a) xs

buildEqs ctx (IfElse b e1 e2) t = let (a, ctx') = freshTVar ctx
                                   in
                                     Data.Set.singleton (TEQ t a) `Data.Set.union` buildEqs ctx' b TBool `Data.Set.union` buildEqs ctx' e1 a `Data.Set.union` buildEqs ctx e2 a
buildEqs _ Leq t = Data.Set.singleton $ TEQ t (TFun TInt (TFun TInt TBool))
buildEqs _ Eq t = Data.Set.singleton $ TEQ t (TFun TInt (TFun TInt TBool))
buildEqs ctx e t = error $ show e

binaryOpType :: Type -> Type
binaryOpType t = TFun t (TFun t t)

unifyAll :: Data.Set.Set TEQ -> Either Substitution TypeError
unifyAll = Data.Set.foldl (\x (TEQ t1 t2) -> onSuccess x $ \s ->
                                              onSuccess (unify (TEQ (apply s t1) (apply s t2))) $ \s' ->
                                                Left $ compose s' s) (Left (Data.Map.empty))

type Substitution = Map Int Type

data TEQ = TEQ Type Type
  deriving (Eq, Ord)

instance Show TEQ where
  show (TEQ t1 t2) = show t1 ++ " = " ++ show t2

apply :: Substitution -> Type -> Type
apply s t@(TVar x) = Data.Map.findWithDefault t x s
apply _ TInt = TInt
apply _ TBool = TBool
apply s (TList t) = TList (apply s t)
apply s (TFun t1 t2) = TFun (apply s t1) (apply s t2)

compose :: Substitution -> Substitution -> Substitution
compose s1 s2 = (Data.Map.filterWithKey (\i t -> case t of TVar j -> i /= j; _ -> True)) $ Data.Map.map (apply s1) s2 `Data.Map.union` s1

onSuccess :: Either Substitution TypeError -> (Substitution -> Either Substitution TypeError) -> Either Substitution TypeError
onSuccess r@(Right  _) _ = r
onSuccess (Left l) f = f l

unify :: TEQ -> Either Substitution TypeError
unify (TEQ t1@(TVar x) t2) = if not $ x `Data.Set.member` ftv t2 then
                               Left $ Data.Map.singleton x t2
                             else if t1 == t2 then
                               Left $ Data.Map.empty
                             else
                               Right $ TE t1 t2
unify (TEQ TInt TInt) = Left $ Data.Map.empty
unify (TEQ TBool TBool) = Left $ Data.Map.empty

unify (TEQ t x@(TVar _)) = unify (TEQ x t)
unify (TEQ (TFun t1 t2) (TFun t1' t2')) = let s2 = unify (TEQ t2 t2') in
                                           onSuccess s2 $ \s ->
                                             onSuccess (unify (TEQ (apply s t1) (apply s t1'))) $ \s' ->
                                               Left $ compose s' s

unify (TEQ (TList t1) (TList t2)) = unify (TEQ t1 t2)
unify (TEQ t1 t2) = Right $ TE t1 t2

{-




inferType :: TypeContext -> Exp -> (Type, TypeContext)
inferType ctx (Var x) = case Data.Map.lookup x ctx of
  Just t -> (t, empty)
  Nothing -> error $ "Variable not in context: " ++ x

inferType ctx (Abs x e) = let tx = freshTVar ctx
                              (te, ctx') = inferType (insert x tx ctx) e
                           in
                            (TFun (findWithDefault tx x ctx')  te, ctx')

inferType ctx (App e1 e2) = let (t1,subst1) = inferType ctx e1
                                ctx' = applyCtx subst1 ctx
                                (t2,subst2) = inferType ctx' e2
                                ctx'' = applyCtx subst2 ctx'
                                a1:a2:_ = freshTVars ctx''
                                subst = unify [(TFun a1 a2, t1), (a1,t2)] `union` subst1 `union` subst2
                             in
                                (apply subst a2, subst)

inferType ctx Add = (TFun TInt (TFun TInt TInt), empty)
inferType ctx Sub = (TFun TInt (TFun TInt TInt), empty)
inferType ctx (Numeral _) = (TInt, empty)
inferType ctx (Boolean _) = (TBool, empty)
inferType ctx Cons = let a1:_ = freshTVars ctx in
  ((TFun a1 (TFun (TList a1) (TList a1))), empty)

inferType ctx (List es) = let ts = map (inferType ctx) es
                           in
                              -- TODO
                              (TList TInt, empty)
inferType ctx Foldr = let a:b:_ = freshTVars ctx in
  (TFun (TFun a (TFun b b)) (TFun b (TFun (TList a) b)), empty)

inferType ctx (IfElse b e1 e2) = let (bt,sb) = inferType ctx b
                                     (e1t,se1) = inferType ctx e1
                                     (e2t,se2) = inferType ctx e2
                                  in
                                    (e1t, unify [(bt, TBool), (e1t, e2t)] `union` sb `union` se1 `union` se2)
inferType ctx And = (TFun TBool (TFun TBool TBool), empty)
inferType ctx Or = (TFun TBool (TFun TBool TBool), empty)
inferType ctx Leq = (TFun TInt (TFun TInt TBool), empty)
inferType ctx Eq = (TFun TInt (TFun TInt TBool), empty)
inferType ctx e = error $ "Cannot typecheck term: " ++ show e

unify :: [(Type, Type)] -> Substitution
unify [] = empty
unify ((t1,t2):rest) = let subst = unify rest
                        in subst `union` unifyOne (apply subst t1) (apply subst t2)
                      

unifyOne :: Type -> Type -> Substitution
unifyOne (TVar x) (TVar y) = if x == y then empty else singleton x (TVar y)
unifyOne (TVar x) t = singleton x t -- TODO check for free vars, x must not be (free) in t
unifyOne t (TVar x) = singleton x t -- TODO check for free vars, x must not be (free) in t
unifyOne (TFun t1 t2) (TFun t1' t2') = unify [(t1,t1'), (t2,t2')]
unifyOne TInt TInt = empty
unifyOne TBool TBool = empty
unifyOne (TList t1) (TList t2) = unifyOne t1 t2
unifyOne t1 t2 = error $ "Cannot unify types: " ++ show t1 ++ ", " ++ show t2

apply :: Substitution -> Type -> Type
apply subst t@(TVar x) = findWithDefault t x subst
apply subst (TFun t1 t2) = TFun (apply subst t1) (apply subst t2)
apply subst TInt = TInt
apply subst TBool = TBool
apply subst (TList t) = TList (apply subst t)

applyCtx :: Substitution -> TypeContext -> TypeContext
applyCtx subst = Data.Map.map (apply subst)

-}
