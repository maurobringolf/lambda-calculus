module ChurchEncoding.TypeChecker.TypeChecker where

import ChurchEncoding.TypeChecker.TypeContext
import ChurchEncoding.TypeChecker.Type(Type(..), ftv, getArgs)
import ChurchEncoding.Ast
import Data.Map(Map)
import qualified Data.Map
import qualified Data.Set
import Control.Monad(forM_, forM, foldM)

data TypeError = TE Type Type
  deriving (Show, Eq)

getMainType :: Program -> Type
getMainType p = case Data.Map.lookup "main" (typeCheck p) of
                  Nothing -> error $ "Missing main function."
                  Just t -> t

typeCheck :: Program -> TypeContext
typeCheck (P funDefs dataDefs) = runWithTypeContext (do
  forM_ funDefs $ \(Def d e) -> do
    ctx <- getCtx
    insertCtx d (inferTypeNamed d e ctx)
  getCtx) (foldr insertConsTypes (Data.Map.singleton "undefined" $ TVar 0) dataDefs)

insertConsTypes :: DataDef -> TypeContext -> TypeContext
insertConsTypes (DDef n conss) ctx = foldr (\cons ctx -> (Data.Map.insert (getConsName cons) (inferConsType n cons) ctx)) ctx conss

inferConsType :: String -> Exp -> Type
inferConsType adt cons = foldr (\x c -> TFun x c) (ADT adt) (go cons)
  where
    go e = case e of
      (Constructor "Int") -> [TInt]
      (Constructor "Bool") -> [TBool]
      -- TODO what about other ADT types nested inside an ADT? Need to distinguish constructors of current type from type names of other ADTs
      -- TODO what about lists
      (Constructor n) -> if n == adt then [ADT adt] else []
      (App lhs rhs) -> let lhsT = go lhs
                           rhsT = go rhs
                        in
                          lhsT ++ rhsT
      x -> error $ show x

inferTypeNamed :: String -> Exp -> TypeContext -> Type
inferTypeNamed n e = runWithTypeContext $ do
  a <- freshTVar
  withShadow n a $ do
    eqs <- buildEqs e a
    let s = unifyAll eqs
    return $ case s of
      Right s -> apply s a
      Left r -> error $ show r

inferType :: Exp -> TypeContext -> Type
inferType e = runWithTypeContext $ do
  a <- freshTVar
  eqs <- buildEqs e a
  let s = unifyAll eqs
  return $ case s of
    Right s -> apply s a
    Left r -> error $ show r

buildEqs :: Exp -> Type -> WithTypeContext (Data.Set.Set TEQ)
buildEqs (Var x) t = do ctx <- getCtx
                        return $ case Data.Map.lookup x ctx of
                          Nothing -> error $ "Variable " ++ x ++ " not in scope"
                          Just t' -> if t' == t then Data.Set.empty else Data.Set.singleton $ TEQ t' t
buildEqs (Abs x e) t = do a1 <- freshTVar
                          a2 <- freshTVar
                          eqs <- withShadow x a1 $ buildEqs e a2
                          return $ Data.Set.insert (TEQ (TFun a1 a2) t) eqs

buildEqs (App t1 t2) t = do a <- freshTVar
                            eqs1 <- buildEqs t1 (TFun a t)
                            eqs2 <- buildEqs t2 a
                            return $ eqs1 `Data.Set.union` eqs2

buildEqs Add t = return $ Data.Set.singleton $ TEQ t $ binaryOpType TInt
buildEqs Sub t = return $ Data.Set.singleton $ TEQ t $ binaryOpType TInt
buildEqs Mult t = return $ Data.Set.singleton $ TEQ t $ binaryOpType TInt
buildEqs Foldr t = do a1 <- freshTVar
                      a2 <- freshTVar
                      return $ Data.Set.singleton $ TEQ t (TFun (TFun a1 (TFun a2 a2)) (TFun a2 (TFun (TList a1) a2)))
buildEqs Tail t = do a <- freshTVar
                     return $ Data.Set.singleton $ TEQ t (TFun (TList a) (TList a))

buildEqs (Numeral _) t = return $ Data.Set.singleton $ TEQ TInt t
buildEqs (Boolean _) t = return $ Data.Set.singleton $ TEQ TBool t
buildEqs And t = return $ Data.Set.singleton $ TEQ t $ binaryOpType TBool
buildEqs Or t = return $ Data.Set.singleton $ TEQ t $ binaryOpType TBool
buildEqs Cons t = do a <- freshTVar
                     return $ Data.Set.singleton $ TEQ t (TFun a (TFun (TList a) (TList a)))
buildEqs (List xs) t = do a <- freshTVar
                          Data.Set.unions <$> (Data.Set.singleton (TEQ t (TList a)):) <$> (forM xs $ \x -> buildEqs x a)
buildEqs (IfElse b e1 e2) t = do bEqs <- buildEqs b TBool
                                 eqs1 <- buildEqs e1 t
                                 eqs2 <- buildEqs e2 t
                                 return $ bEqs `Data.Set.union` eqs1 `Data.Set.union` eqs2
buildEqs Eq t = return $ Data.Set.singleton $ TEQ t (TFun TInt (TFun TInt TBool))
buildEqs Leq t = return $ Data.Set.singleton $ TEQ t (TFun TInt (TFun TInt TBool))
buildEqs Geq t = return $ Data.Set.singleton $ TEQ t (TFun TInt (TFun TInt TBool))
-- TODO add typechecking for patterns and 'e'
buildEqs (CaseOf e ps) t = Data.Set.unions <$> (forM ps $ \(p,x) -> do
                                                  let vars = getPatternVars p
                                                      cons = getConsName p
                                                  ctx <- getCtx
                                                  let consT = case Data.Map.lookup cons ctx of
                                                                Nothing -> error $ "Unknown constructor " ++ cons ++ " in pattern."
                                                                Just consT -> consT
                                                      argTypes = getArgs consT

                                                  if length argTypes /= length vars then
                                                    error $ "Incorrect number of arguments in pattern " ++ show p
                                                  else do
                                                    withShadows (zip vars argTypes) (buildEqs x t))
buildEqs (Constructor cons) t' = do ctx <- getCtx
                                    return $ case Data.Map.lookup cons ctx of
                                      Nothing -> error $ "Constructor " ++ cons ++ " not in scope."
                                      Just t -> Data.Set.singleton $ TEQ t t'
buildEqs e _ = error $ show e


binaryOpType :: Type -> Type
binaryOpType t = TFun t (TFun t t)

unifyAll :: Data.Set.Set TEQ -> Either TypeError Substitution
unifyAll = foldM (\s eq -> unify (applyTEQ s eq) >>= return . flip compose s) Data.Map.empty

type Substitution = Map Int Type

data TEQ = TEQ Type Type
  deriving (Eq, Ord)

applyTEQ :: Substitution -> TEQ -> TEQ
applyTEQ s (TEQ t1 t2) = TEQ (apply s t1) (apply s t2)

instance Show TEQ where
  show (TEQ t1 t2) = show t1 ++ " = " ++ show t2

apply :: Substitution -> Type -> Type
apply s t@(TVar x) = Data.Map.findWithDefault t x s
apply _ TInt = TInt
apply _ TBool = TBool
apply s (TList t) = TList (apply s t)
apply s (TFun t1 t2) = TFun (apply s t1) (apply s t2)
apply s (ADT n) = ADT n

compose :: Substitution -> Substitution -> Substitution
compose s1 s2 = (Data.Map.filterWithKey (\i t -> case t of TVar j -> i /= j; _ -> True)) $ Data.Map.map (apply s1) s2 `Data.Map.union` s1

onSuccess :: Either TypeError Substitution -> (Substitution -> Either TypeError Substitution) -> Either TypeError Substitution
onSuccess r@(Left  _) _ = r
onSuccess (Right l) f = f l

unify :: TEQ -> Either TypeError Substitution
unify (TEQ t1@(TVar x) t2) = if not $ x `Data.Set.member` ftv t2 then
                               Right $ Data.Map.singleton x t2
                             else if t1 == t2 then
                               Right $ Data.Map.empty
                             else
                               Left $ TE t1 t2
unify (TEQ TInt TInt) = Right $ Data.Map.empty
unify (TEQ TBool TBool) = Right $ Data.Map.empty

unify (TEQ t x@(TVar _)) = unify (TEQ x t)
unify (TEQ (TFun t1 t2) (TFun t1' t2')) = do s <- unify (TEQ t2 t2')
                                             s' <- unify $ applyTEQ s $ TEQ t1 t1'
                                             return $ compose s' s

unify (TEQ (TList t1) (TList t2)) = unify (TEQ t1 t2)
unify (TEQ (ADT n) (ADT m)) = if n == m then Right Data.Map.empty else Left $ TE (ADT n) (ADT m)
unify (TEQ t1 t2) = Left $ TE t1 t2
