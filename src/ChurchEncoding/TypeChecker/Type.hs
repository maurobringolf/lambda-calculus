module ChurchEncoding.TypeChecker.Type where

import qualified Data.Set
import qualified Data.Map

data Type = TInt
          | TBool
          | TFun Type Type
          | TVar Int
          | TList Type
          | ADT String
  deriving (Eq, Ord)

equalsUpToRenaming :: Type -> Type -> Bool
equalsUpToRenaming t1 t2 = case equalsWithRenameLeft t1 t2 Data.Map.empty of
    Nothing -> False
    Just _ -> True

equalsWithRenameLeft :: Type -> Type -> Data.Map.Map Int Int -> Maybe (Data.Map.Map Int Int)
equalsWithRenameLeft t1 t2 r = case (t1,t2) of
  (TInt,TInt) -> Just r
  (TBool,TBool) -> Just r
  (ADT n, ADT m) -> if n == m then Just r else Nothing
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
  show (ADT n) = n

ftv :: Type -> Data.Set.Set Int
ftv (TVar i) = Data.Set.singleton i
ftv (TFun t1 t2) = Data.Set.union (ftv t1) (ftv t2)
ftv TInt = Data.Set.empty
ftv TBool = Data.Set.empty
ftv (TList t) = ftv t
ftv (ADT _) = Data.Set.empty

getArgs :: Type -> [Type]
getArgs (TFun t1 t2) = t1 : getArgs t2
getArgs _ = []
