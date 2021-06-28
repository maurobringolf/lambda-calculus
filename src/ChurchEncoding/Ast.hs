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
  deriving (Show, Eq)

data Definition = Def String Exp
  deriving (Eq)

data Program = P [Definition]
  deriving (Eq)

instance Show Program where
  show (P defs) = intercalate "\n" $ map show defs

instance Show Definition where
  show (Def name body) = name ++ " = " ++ show body

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
  List xs -> foldr Data.Set.union Data.Set.empty $ map freeVars xs
  Sub -> Data.Set.empty
  Eq -> Data.Set.empty
  Leq -> Data.Set.empty
  Geq -> Data.Set.empty
  Mult -> Data.Set.empty
  IfElse b e1 e2 -> freeVars b `Data.Set.union` freeVars e1 `Data.Set.union` freeVars e2
