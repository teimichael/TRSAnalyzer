module Core.TRS
    ( Term (..),
      Rule (..),
      TRS (..),
      showRule,
      showTRS,
      variables,
      substitute,
      extraVariables,
      variableCondition,
      substituteTRS,
      variablesTRS
    ) where

import Data.List ( intercalate, nub )

data Term     = V String | F String [Term] deriving (Eq)
type Subst    = [(String, Term)]
type Rule     = (Term, Term)
type TRS      = [Rule]

instance Show Term where
  show (V x)    = x
  show (F f ts) = f ++ "(" ++ intercalate "," [show t | t <- ts] ++ ")"

showRule :: Rule -> String
showRule (l, r) = show l ++ " -> " ++ show r

showTRS :: TRS -> String
showTRS trs =
    unlines (("(" ++ intercalate " " ("VAR" : variablesTRS trs) ++ ")") :
             "(RULES" : [ showRule rule | rule <- trs ] ++ [")"])

variables :: Term -> [String]
variables (V x) = [x]
variables (F _ ts) = nub [ x | t <- ts, x <- variables t ]

substitute :: Term -> Subst -> Term
substitute (V x) sigma
    | Just t <- lookup x sigma = t
    | otherwise                = V x
substitute (F f ts) sigma      = F f [ substitute t sigma | t <- ts ]

extraVariables :: Rule -> [String]
extraVariables (l, r) = nub [ x | x <- variables r, not (elem x (variables l)) ]

variableCondition :: TRS -> Bool
variableCondition trs = all (\rule -> extraVariables rule == []) trs

variablesTRS :: TRS -> [String]
variablesTRS trs =
    nub [ x | (l, r) <- trs, t <- [l, r], x <- variables t ]

substituteTRS :: TRS -> Subst -> TRS
substituteTRS trs sigma =
    [ (substitute l sigma, substitute r sigma) | (l, r) <- trs ]