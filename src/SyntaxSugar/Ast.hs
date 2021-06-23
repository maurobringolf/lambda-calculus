module SyntaxSugar.Ast where

import Data.List(intercalate)

data Exp = Var String
         | Abs String Exp
         | App Exp Exp
         | Numeral Integer
         | Add
         | Sub
         | Mult
  deriving (Show, Eq)

data Definition = Def String Exp
  deriving (Eq)

data Program = P [Definition]
  deriving (Eq)

instance Show Program where
  show (P defs) = intercalate "\n" $ map show defs

instance Show Definition where
  show (Def name body) = name ++ " = " ++ show body
