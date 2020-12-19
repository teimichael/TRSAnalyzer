module Termination.Main
    ( mainFunc,
      testFunc
    ) where

import System.Environment ( getArgs )  
import System.Exit ( ExitCode(ExitFailure), exitWith )
import Parser ( readTRSFile )
import Core.TRS
    ( TRS, Rule, Term(..), showTRS, variableCondition )
import Termination.Lib ( linearInterpretation )


{-
    Test case
-}

r1 :: Rule
r1 = (F "a" [F "n" [], V "ys"], V "ys")

r2 :: Rule
r2 = (F "a" [F "c" [V "x", V "xs"], V "ys"], F "c" [V "x", F "a" [V "xs", V "ys"]])

trs :: TRS
trs = [r1, r2]

showProver :: TRS -> String
showProver trs = unlines $ map show (linearInterpretation trs)

testFunc :: IO ()
testFunc = do
    putStrLn $ showProver trs

mainFunc :: IO()
mainFunc = do
    file : _  <- getArgs
    result <-  readTRSFile file
    case result of
      Left e -> do
        print e
        exitWith (ExitFailure 1)
      Right trs -> 
        if not (variableCondition trs) then do
           putStrLn "ERROR"
           putStrLn "The TRS violates the variable condition."
        else do
           putStr (showProver trs)