module Termination.Lib
    ( linearInterpretation
    ) where

import Data.List ( elemIndex, nub )
import Data.Maybe ( fromJust )
import Core.SMT ( Expr(..), Formula (..), Command (..) )
import Core.TRS ( Rule, TRS, Term(..) )

{-
    Command construction
-}
-- Polynomial interpretation method (linear-form)
linearInterpretation :: TRS -> [Command]
linearInterpretation trs = declareConCoeVars trs ++ [assertFormulae trs] ++ checkCommand trs


-- Declare commands
declareConCoeVars :: TRS -> [Command]
declareConCoeVars trs = map Declare (allConCoeVars trs)


-- Assert command
assertFormulae :: TRS -> Command
assertFormulae trs = Assert $ And $ nub $ concat [ ruleFormulae r | r <- trs]


-- Check commands
checkCommand :: TRS -> [Command]
checkCommand trs = [Check, GetVal $ allConCoeVars trs]

{-
    Formulae
-}

-- Rule formula = Coefficient formulae + Constant formula
ruleFormulae :: Rule -> [Formula]
ruleFormulae r = natFormulae r ++ monoFormulae r ++ coeFormulae r ++ [conFormula r]


-- Natural number formulae
natFormulae :: Rule -> [Formula]
natFormulae (t1, t2) = [Geq conVar (Val 0) | conVar <- nub (conVars t1 ++ conVars t2)]


-- Monotonicity formulae
monoFormulae :: Rule -> [Formula]
monoFormulae (t1, t2) = [Gt coeVar (Val 0) | coeVar <- nub (coeVars t1 ++ coeVars t2)]


-- Coefficient formulae
coeFormulae :: Rule -> [Formula]
coeFormulae (t1, t2) = [Geq (coefficient t1 tVar) (coefficient t2 tVar)  | tVar <- nub (terVars t1 ++ terVars t2)]


-- Constant formula
conFormula :: Rule -> Formula
conFormula (t1, t2) = Gt (constant t1) (constant t2)


{-
    Variables
-}

-- Const variables. E.g. a_0 of (a_0 + a_1 * x_1)
conVars :: Term -> [Expr]
conVars (V _) = []
conVars (F s as) = Var (s ++ "_0") : concat [conVars a | a <- as]


-- Coefficient variables. E.g. a_1 of (a_0 + a_1 * x_1)
coeVars :: Term -> [Expr]
coeVars (V _) = []
coeVars (F s as) = [Var (s ++ "_" ++ show i) | i <- [1..length as]] ++ concat [coeVars a | a <- as]


-- Constant and Coefficient variables. E.g. a_0, a_1 of (a_0 + a_1 * x_1)
conCoeVars :: Term -> [Expr]
conCoeVars (V _) = []
conCoeVars (F s as) = [Var (s ++ "_" ++ show i) | i <- [0..length as]] ++ concat [conCoeVars a | a <- as]


-- All Constant and Coefficient variables in a TRS
allConCoeVars :: TRS -> [Expr]
allConCoeVars trs = nub $ concat [ conCoeVars t1 ++ conCoeVars t2 | (t1, t2) <- trs]


-- Term variables. E.g. x_1 of (a_0 + a_1 * x_1)
terVars :: Term -> [String]
terVars (V v) = [v]
terVars (F _ as) = concat [ terVars a | a <- as]


{-
    Calculate coefficients and constants
-}
-- Coefficient of a term variable
coefficient :: Term -> String -> Expr
coefficient (V v) x | v == x = Val 1
                    | otherwise = Val 0
coefficient (F _ []) _ = Val 0
coefficient (F s [V v]) x | v == x = Var (s ++ "_1")
                          | otherwise = Val 0
coefficient (F s as) x = simplify $ Plus [Mul [Var (s ++ "_" ++ show (index a)), coefficient a x] | a <- as]
    where index a = fromJust (a `elemIndex` as) + 1


-- Constant of a term
constant :: Term -> Expr
constant (V _) = Val 0
constant (F s []) = Var (s ++ "_0")
constant (F s [V _]) = Var (s ++ "_0")
constant (F s as) = simplify $ Plus (Var (s ++ "_0") : [Mul [Var (s ++ "_" ++ show (index a)), constant a] | a <- as])
    where index a = fromJust (a `elemIndex` as) + 1


-- Simplify expressions
simplify :: Expr -> Expr
simplify (Val v) = Val v
simplify (Var v) = Var v
simplify (Mul es) | Val 0 `elem` es = Val 0
                  | otherwise = simMul $ filter (\e' -> e' /= Val 1) [simplify e | e <- es]
    where simMul [] = Val 0
          simMul [e] = e
          simMul es | Val 0 `elem` es || Val 1 `elem` es = simplify (Mul es)
                    | otherwise = Mul es
simplify (Plus es) = simPlus $ filter (\e -> e /= Val 0) [simplify e | e <- es]
    where simPlus [] = Val 0
          simPlus [e] = e
          simPlus es | Val 0 `elem` es = simplify (Plus es)
                     | otherwise = Plus es
