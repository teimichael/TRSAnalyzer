module Complexity.Lib
    ( linearInterpretation
    ) where

import Data.List ( elemIndex, nub )
import Data.Maybe (fromJust)
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
assertFormulae trs = Assert $ And $ nub $ concat [ ruleFormulae trs r | r <- trs]


-- Check commands
checkCommand :: TRS -> [Command]
checkCommand trs = [Check, GetVal $ allConCoeVars trs]

{-
    Formulae
-}

-- Rule formula
ruleFormulae :: TRS -> Rule -> [Formula]
ruleFormulae trs r = natFormulae r ++ monoFormulae trs r ++ coeFormulae trs r ++ [conFormula trs r]


-- Natural number formulae (all constant SMT variables >= 0)
natFormulae :: Rule -> [Formula]
natFormulae (t1, t2) = [Geq conVar (Val 0) | conVar <- nub (conVars t1 ++ conVars t2)]


-- Monotonicity formulae (all coefficient SMT variables > 0)
monoFormulae :: TRS -> Rule -> [Formula]
monoFormulae trs (t1, t2) = [Gt coeVar (Val 0) | coeVar <- nub (coeVars trs t1 ++ coeVars trs t2)]


-- Coefficient formulae (the left coefficient SMT variables >= the right coefficient SMT variables) for each term variable in left and right terms
coeFormulae :: TRS -> Rule -> [Formula]
coeFormulae trs (t1, t2) = [Geq (restrictConstructorSymbol trs $ coefficient t1 tVar) (restrictConstructorSymbol trs $ coefficient t2 tVar)  | tVar <- nub (terVars t1 ++ terVars t2)]


-- Constant formula (the left constant SMT variables > the right constant SMT variables)
conFormula :: TRS -> Rule -> Formula
conFormula trs (t1, t2) = Gt (restrictConstructorSymbol trs $ constant t1) (restrictConstructorSymbol trs $ constant t2)

{-
    Defined Symbols
-}

definedSymbol :: Rule -> String
definedSymbol (left, _) = rootSymbol left
    where rootSymbol (V _) = ""
          rootSymbol (F s _) = s

allDefinedSymbols :: TRS -> [String]
allDefinedSymbols trs = filter (/= "") $ nub [definedSymbol rule | rule <- trs]

isDefinedSymbol :: TRS -> String -> Bool
isDefinedSymbol trs s = s `elem` allDefinedSymbols trs

{-
    SMT Variables
-}

-- Constant SMT variables. E.g. a_0 of (a_0 + a_1 * x_1)
conVars :: Term -> [Expr]
conVars (V _) = []
conVars (F s as) = Var (s ++ "_0") : concat [conVars a | a <- as]


-- Coefficient SMT variables. E.g. a_1 of (a_0 + a_1 * x_1)
coeVars :: TRS -> Term -> [Expr]
coeVars trs (V _) = []
coeVars trs (F s as) | isDefinedSymbol trs s = [Var (s ++ "_" ++ show i) | i <- [1..length as]] -- s must be a defined symbol
                     | otherwise = []



-- All Constant and Coefficient SMT variables in a TRS
allConCoeVars :: TRS -> [Expr]
allConCoeVars trs = nub $ concat [ conVars t1 ++ coeVars trs t1 ++ conVars t2 ++ coeVars trs t2 | (t1, t2) <- trs]


{-
    Term variables. E.g. x_1 of (a_0 + a_1 * x_1)
-}
terVars :: Term -> [String]
terVars (V v) = [v]
terVars (F _ as) = concat [ terVars a | a <- as]


{-
    Calculate coefficient and constant expressions
-}
-- Calculate coefficient expression of a given variable for a term
coefficient :: Term -> String -> Expr
coefficient (V v) x | v == x = Val 1
                    | otherwise = Val 0
coefficient (F _ []) _ = Val 0
coefficient (F s [V v]) x | v == x = Var (s ++ "_1")
                          | otherwise = Val 0
coefficient (F s as) x = simplify $ Plus [Mul [Var (s ++ "_" ++ show (index a)), coefficient a x] | a <- as]
    where index a = fromJust (a `elemIndex` as) + 1

-- Deprecated implementation
-- coefficient :: TRS -> Term -> String -> Expr
-- coefficient _ (V v) x | v == x = Val 1
--                     | otherwise = Val 0
-- coefficient _ (F _ []) _ = Val 0
-- coefficient trs (F s [V v]) x | isDefinedSymbol trs s && v == x = Var (s ++ "_1") -- s is defined symbol and variable name matches
--                               | v == x = Val 1 -- s is not defined symbol and variable name matches
--                               | otherwise = Val 0
-- coefficient trs (F s as) x | isDefinedSymbol trs s = simplify $ Plus [Mul [Var (s ++ "_" ++ show (index a)), coefficient trs a x] | a <- as] -- s is defined symbol
--                            | otherwise = simplify $ Plus [Mul [Val 1, coefficient trs a x] | a <- as] -- s is not defined symbol
--     where index a = fromJust (a `elemIndex` as) + 1


-- Calculate constant expression for a term
constant :: Term -> Expr
constant (V _) = Val 0
constant (F s []) = Var (s ++ "_0")
constant (F s [V _]) = Var (s ++ "_0")
constant (F s as) = simplify $ Plus (Var (s ++ "_0") : [Mul [Var (s ++ "_" ++ show (index a)), constant a] | a <- as])
    where index a = fromJust (a `elemIndex` as) + 1

{-
add(s(x), y) -> s(add(x, y))

constant (F "add" [F "s" [V "x"], V "y"])
s = "add"
as = [F "s" [V "x"], V "y"]

Plus (Var "add_0" : [Var ("add_" ++ show (index F "s" [V "x"])) * constant F "s" [V "x"], Var ("add_" ++ show (index V "y")) * constant V "y"])
index F "s" [V "x"] = fromJust (F "s" [V "x"] `elemIndex` [F "s" [V "x"], V "y"]) + 1 = 1
- F "s" [V "x"] `elemIndex` [F "s" [V "x"], V "y"] -> Just 0
- fromJust (Just 0) -> 0
- show 1 = "1"

Plus (Var "add_0" : [Var ("add_1") * constant F "s" [V "x"], Var ("add_2") * constant V "y"])
Plus (Var "add_0" : [Var "add_1" * "s_0", Var "add_2" * Val 0])
add_0 + add_1 * s_0 + add_2 * Val 0
simplify add_0 + add_1 * s_0 + add_2 * Val 0 -> add_0 + add_1 * s_0
-}


-- Restrict constructor symbol (replace coefficients of constructor symbols with 1)
restrictConstructorSymbol :: TRS -> Expr -> Expr
restrictConstructorSymbol trs (Val v) = Val v
restrictConstructorSymbol trs (Var v) | isDefinedSymbol trs $ symbolOf v = Var v  -- If is defined symbol
                                      | indexOf v == "0" = Var v -- If is constant variable (index = 0)
                                      | otherwise = Val 1
    where symbolOf v =
            case span (/='_') (reverse v) of
            (afterUnderscoreRev, _ : beforeUnderscoreRev) -> reverse beforeUnderscoreRev
            (withoutUnderscoreRev, []) -> reverse withoutUnderscoreRev
          indexOf v = 
            case span (/='_') (reverse v) of
                (afterUnderscoreRev, _ : beforeUnderscoreRev) -> reverse afterUnderscoreRev
                (withoutUnderscoreRev, []) -> reverse withoutUnderscoreRev
restrictConstructorSymbol trs (Plus es) = Plus [restrictConstructorSymbol trs e | e <- es]
restrictConstructorSymbol trs (Sub es) = Sub [restrictConstructorSymbol trs e | e <- es]
restrictConstructorSymbol trs (Mul es) = Mul [restrictConstructorSymbol trs e | e <- es]


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
