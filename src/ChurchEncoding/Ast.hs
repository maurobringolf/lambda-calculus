module ChurchEncoding.Ast where

import Data.List(intercalate)
import qualified Data.Set

data Exp = Var String
         | Abs String Exp
         | App Exp Exp
         | Numeral Integer
         | Boolean Bool
         | And
         | Or
         | Add
         | Eq
         | Leq
         | Geq
         | Sub
         | Mult
         | IfElse Exp Exp Exp
         | List [Exp]
         | Cons
         | Foldr
         | Tail
         | Constructor String
         | CaseOf Exp [(Exp, Exp)]
  deriving (Show, Eq, Ord)

getConsName :: Exp -> String
getConsName cons = case cons of
  (Constructor c) -> c
  (App lhs rhs) -> getConsName lhs

data FunDef = Def String Exp
  deriving (Eq)

data DataDef = DDef String [Exp]
  deriving (Eq)

data Program = P { funsDefs :: [FunDef]
                 , dataDefs :: [DataDef] }
  deriving (Eq)

-- TODO show dataDefs
instance Show Program where
  show (P funDefs dataDefs) = intercalate "\n" $ map show funDefs

instance Show FunDef where
  show (Def name body) = name ++ " = " ++ show body

getPatternVars :: Exp -> [String]
getPatternVars p = case p of
  (Var x) -> [x];
  App p1 p2 -> getPatternVars p1 ++ getPatternVars p2
  Constructor _ -> []

freeVars :: Exp -> Data.Set.Set String
freeVars e = case e of
  Var x -> Data.Set.singleton x
  App e1 e2 -> freeVars e1 `Data.Set.union` freeVars e2
  Abs x t -> Data.Set.delete x (freeVars t)
  Numeral _ -> Data.Set.empty
  Boolean _ -> Data.Set.empty
  Add -> Data.Set.empty
  Or -> Data.Set.empty
  And -> Data.Set.empty
  Cons -> Data.Set.empty
  Foldr -> Data.Set.empty
  Tail -> Data.Set.empty
  List xs -> foldr Data.Set.union Data.Set.empty $ map freeVars xs
  Sub -> Data.Set.empty
  Eq -> Data.Set.empty
  Leq -> Data.Set.empty
  Geq -> Data.Set.empty
  Mult -> Data.Set.empty
  IfElse b e1 e2 -> freeVars b `Data.Set.union` freeVars e1 `Data.Set.union` freeVars e2
  CaseOf e ps -> Data.Set.unions (freeVars e: map (\(p,x) -> freeVars x `Data.Set.difference` freeVars p) ps)
  Constructor _ -> Data.Set.empty
