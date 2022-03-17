module Main where

-- import Lib

import Args
import Control.Monad
import Data.Maybe
import System.Environment
import System.Exit
import Utils (printLine)
import Generation (getGenerationTable)

main :: IO ()
main = do
    args <- getArgs
    let parsedArgs = verifArgs $ parseArgs args createTemplateArgs
    when (isNothing parsedArgs) $ putStrLn "Error : bad arguments"
        >> exitWith (ExitFailure 84)
    mapM_ (printLine (fromJust (window (fromJust parsedArgs))))
        (drop (fromJust (start (fromJust parsedArgs)))
        (getGenerationTable (fromJust parsedArgs)))
