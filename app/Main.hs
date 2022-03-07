module Main where

-- import Lib

import Args
import Control.Monad
import Data.Maybe
import System.Environment
import System.Exit
import Utils (fillEmptyBits, getBit, intToStringBits)

createEasyTest :: Args
createEasyTest = Args {
    rule = Nothing,
    start = Nothing,
    Args.lines = Nothing,
    window = Nothing,
    move = Nothing}

readInt :: [Char] -> Maybe Int
readInt [] = Nothing
readInt string =
    case reads string of
        [(x, "")] -> Just x
        _ -> Nothing

type Cell = Char

data RuleCase = RuleCase {
    setup :: [Char],
    res :: Cell}

data RuleDef = RuleDef {
    byte0 :: RuleCase,
    byte1 :: RuleCase,
    byte2 :: RuleCase,
    byte3 :: RuleCase,
    byte4 :: RuleCase,
    byte5 :: RuleCase,
    byte6 :: RuleCase,
    byte7 :: RuleCase}

createRuleCase :: Args -> Int -> RuleCase
createRuleCase args n = RuleCase {
    setup = [lc, mc, rc],
    res = if getBit (7 - n) (fillEmptyBits(intToStringBits
        (fromJust (rule args))) 8) == '1' then '*' else ' '}
    where
        lc = if getBit 0 (fillEmptyBits (intToStringBits n) 3) == '1'
        then '*' else ' '
        mc = if getBit 1 (fillEmptyBits (intToStringBits n) 3) == '1'
        then '*' else ' '
        rc = if getBit 2 (fillEmptyBits (intToStringBits n) 3) == '1'
        then '*' else ' '

createRule :: Args -> RuleDef
createRule args = RuleDef {
    byte0 = createRuleCase args 0,
    byte1 = createRuleCase args 1,
    byte2 = createRuleCase args 2,
    byte3 = createRuleCase args 3,
    byte4 = createRuleCase args 4,
    byte5 = createRuleCase args 5,
    byte6 = createRuleCase args 6,
    byte7 = createRuleCase args 7}

parseArgs :: [String] -> Args -> Maybe Args
parseArgs ("--rule" : y : zs) args = parseArgs zs args {rule = readInt y}
parseArgs ("--start" : y : zs) args = parseArgs zs (args {start = readInt y})
parseArgs ("--lines" : y : zs) args =
    parseArgs zs (args {Args.lines = readInt y})
parseArgs ("--window" : y : zs) args =
    parseArgs zs (args {window = readInt y})
parseArgs ("--move" : y : zs) args = parseArgs zs (args {move = readInt y})
parseArgs [] args = Just args
parseArgs _ _ = Nothing

verifArgs :: Maybe Args -> Maybe Args
verifArgs Nothing = Nothing
verifArgs (Just (Args Nothing s l w m)) = Nothing
verifArgs (Just (Args r s l w m)) = Just (Args r s' l' w' m')
    where
        s' = if isJust s then s else Just 0
        l' = if isJust l then l else Nothing
        w' = if isJust w then w else Just 80
        m' = if isJust m then m else Just 0

printLine :: Int -> [Char] -> IO ()
printLine _ [] = return ()
printLine 0 _ = putChar '\n'
printLine n (x : xs) =
    if n <= 0
        then return ()
        else putChar x >> printLine (n - 1) xs

generateDefaultLine :: Int -> [Char]
generateDefaultLine n = replicate (div n 2) ' ' ++ "*" ++ repeat ' '

strToCase :: [Char] -> [[Char]]
strToCase [] = []
strToCase (x : y : z : zs) =
    (x : y : [z]) :
    strToCase (y : z : zs)
strToCase _ = []

computeState :: [[Char]] -> RuleDef -> [Char]
computeState [] _ = []
computeState (x : xs) rule = case x of
    "   " -> res (byte0 rule) : nextRuleComp
    "  *" -> res (byte1 rule) : nextRuleComp
    " * " -> res (byte2 rule) : nextRuleComp
    " **" -> res (byte3 rule) : nextRuleComp
    "*  " -> res (byte4 rule) : nextRuleComp
    "* *" -> res (byte5 rule) : nextRuleComp
    "** " -> res (byte6 rule) : nextRuleComp
    "***" -> res (byte7 rule) : nextRuleComp
    _ -> error "Invalid case"
    where
        nextRuleComp = computeState xs rule

invertRule :: RuleDef -> RuleDef
invertRule rule =
    RuleDef {
        byte0 = RuleCase {setup = setup (byte0 rule), res = res (byte0 rule)},
        byte1 = RuleCase {setup = setup (byte1 rule), res = res (byte4 rule)},
        byte2 = RuleCase {setup = setup (byte2 rule), res = res (byte2 rule)},
        byte3 = RuleCase {setup = setup (byte3 rule), res = res (byte6 rule)},
        byte4 = RuleCase {setup = setup (byte4 rule), res = res (byte1 rule)},
        byte5 = RuleCase {setup = setup (byte5 rule), res = res (byte5 rule)},
        byte6 = RuleCase {setup = setup (byte6 rule), res = res (byte3 rule)},
        byte7 = RuleCase {setup = setup (byte7 rule), res = res (byte7 rule)}}

--   printLine (tail line) (fromJust (window args))
wolframEngine :: [Char] -> [Char] -> Args -> RuleDef -> Maybe Int -> [[Char]]
wolframEngine _ _ _ _ (Just 0) = []
wolframEngine line beforeLine args rule (Just n) =
    line : wolframEngine currString newBLine' args rule (Just (n - 1))
    where
    neg = head (computeState
        (strToCase (head (tail line) : beforeLine)) (invertRule rule))
    newBLine' = computeState (strToCase (head (tail line) : beforeLine))
        (invertRule rule)
    currString =  neg : tail (computeState (strToCase (neg : line)) rule)
wolframEngine line beforeLine args rule Nothing =
    line : wolframEngine currString newBLine' args rule Nothing
    where
    neg = head (computeState
        (strToCase (head (tail line) : beforeLine)) (invertRule rule))
    newBLine' = computeState (strToCase (head (tail line) : beforeLine))
        (invertRule rule)
    currString =  neg : tail (computeState (strToCase (neg : line)) rule)

main :: IO ()
main = do
    args <- getArgs
    let parsedArgs = verifArgs $ parseArgs args createEasyTest
    case parsedArgs of
        Nothing ->
            putStrLn "Error : bad arguments" >> exitWith (ExitFailure 84)
        _ -> return ()
    let wLine = generateDefaultLine (fromJust (window (fromJust parsedArgs)))
    let wRule = createRule (fromJust parsedArgs)
    let zTuple = (wLine, repeat ' ')
    let wGen = if isNothing (Args.lines (fromJust parsedArgs)) then Nothing else Just (fromJust (start (fromJust parsedArgs)) + fromJust (Args.lines (fromJust parsedArgs)))
    mapM_ (printLine (fromJust (window (fromJust parsedArgs)))) (drop (fromJust (start (fromJust parsedArgs))) (uncurry wolframEngine zTuple (fromJust parsedArgs) wRule wGen))
