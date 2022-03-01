module Lib
    ( printResult
    ) where

import System.Exit (ExitCode (ExitFailure), exitWith)

printResult :: String -> IO ()
printResult s = do
    putStrLn s
    exitWith (ExitFailure 1)
