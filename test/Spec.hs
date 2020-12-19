import Termination.Main

main :: IO ()
main = do
    putStrLn "TRS Termination Prover"
    Termination.Main.testFunc
