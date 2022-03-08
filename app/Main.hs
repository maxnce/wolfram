module Main where

-- import Lib

import Args
import Control.Monad
import Data.Maybe
import System.Environment
import System.Exit
import Utils (fillEmptyBits, getBit, intToStringBits, readInt)
import Rule (RuleDef, RuleCase, invertRule, createRule, Cell, computeState)

printLine :: Int -> [Char] -> IO ()
printLine _ [] = return ()
printLine 0 _ = putChar '\n'
printLine n line = putStrLn (take n line)

generateDefaultLine :: Int -> [Char]
generateDefaultLine n = replicate (div n 2) ' ' ++ "*" ++ repeat ' '

strToCase :: [Char] -> [[Char]]
strToCase [] = []
strToCase (x : y : z : zs) =
    (x : y : [z]) :
    strToCase (y : z : zs)
strToCase _ = []

--   printLine (tail line) (fromJust (window args))
wolframEngine :: [Char] -> [Char] -> Args -> RuleDef -> Maybe Int -> [[Char]]
wolframEngine _ _ _ _ (Just 0) = []
wolframEngine line beforeLine args rule n =
    line : wolframEngine currString newBLine' args rule nMinusOne
    where
    neg = getFirstCharFromBLine line beforeLine rule
    newBLine' = computeState (strToCase (head (tail line) : beforeLine))
        (invertRule rule)
    currString =  neg : tail (computeState (strToCase (neg : line)) rule)
    nMinusOne = if isJust n then Just (fromJust n - 1) else Nothing

getFirstCharFromBLine :: [Char] -> [Char] -> RuleDef -> Char
getFirstCharFromBLine line beforeLine rule =
    head (computeState
        (strToCase (head (tail line) : beforeLine)) (invertRule rule))

createInitialTuple :: [Char] -> Args -> ([Char], [Char])
createInitialTuple wLine args =
    (if fromJust (move args) > 0 then replicate (
    fromJust (move args)) ' ' ++ wLine else drop
    (abs (fromJust (move args))) wLine, if fromJust
    (move args) > 0 then repeat ' ' else reverse (take
    (abs (fromJust (move args))) wLine) ++ repeat ' ')

main :: IO ()
main = do
    args <- getArgs
    let parsedArgs = verifArgs $ parseArgs args createTemplateArgs
    when (isNothing parsedArgs) $ putStrLn "Error : bad arguments"
        >> exitWith (ExitFailure 84)
    mapM_ (printLine (fromJust (window (fromJust parsedArgs))))
        (drop (fromJust (start (fromJust parsedArgs)))
        (getGenerationTable (fromJust parsedArgs)))


getGenerationTable :: Args -> [[Char]]
getGenerationTable args =
    uncurry wolframEngine zTuple args wRule wGen
    where
        wLine = generateDefaultLine (fromJust (window args))
        wRule = createRule args
        zTuple = createInitialTuple wLine args
        wGen = if isNothing (Args.lines args) then Nothing
            else Just (fromJust (start args) +
            fromJust (Args.lines args))
