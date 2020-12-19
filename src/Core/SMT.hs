module Core.SMT
    ( Expr (..),
      Formula (..),
      Command (..)
    ) where

-- Define expression structure
data Expr =
      Val Int
    | Var String
    | Plus [Expr]
    | Sub [Expr]
    | Mul [Expr]
    deriving Eq

-- Define formula structure
data Formula =
      And [Formula]
    | Or [Formula]
    | Eq Expr Expr
    | Gt Expr Expr
    | Lt Expr Expr
    | Geq Expr Expr
    | Leq Expr Expr
    | Distinct [Expr]
    deriving Eq

-- Define command structure
data Command = 
      Declare Expr
    | Assert Formula
    | Check
    | GetMod
    | GetVal [Expr]


instance Show Expr where
  show (Val v) = show v
  show (Var v) = "_" ++ v
  show (Plus es) = "(+ " ++ unwords (map show es) ++ ")"
  show (Sub es) = "(- " ++ unwords (map show es) ++ ")"
  show (Mul es) = "(* " ++ unwords (map show es) ++ ")"


instance Show Formula where
  show (And fs) = "(and " ++ unwords (map show fs) ++ ")"
  show (Or fs) = "(or " ++ unwords (map show fs) ++ ")"
  show (Eq e0 e1) = "(= " ++ show e0 ++ " " ++ show e1 ++ ")"
  show (Gt e0 e1) = "(> " ++ show e0 ++ " " ++ show e1 ++ ")"
  show (Lt e0 e1) = "(< " ++ show e0 ++ " " ++ show e1 ++ ")"
  show (Geq e0 e1) = "(>= " ++ show e0 ++ " " ++ show e1 ++ ")"
  show (Leq e0 e1) = "(<= " ++ show e0 ++ " " ++ show e1 ++ ")"
  show (Distinct es) = "(distinct " ++ unwords (map show es) ++ ")"
  

instance Show Command where
  show (Declare var) = "(declare-fun " ++ show var ++ " () Int)"
  show (Assert f) = "(assert " ++ show f ++ ")"
  show Check = "(check-sat)"
  show GetMod = "(get-model)"
  show (GetVal es) = "(get-value (" ++ unwords (map show es) ++ "))"