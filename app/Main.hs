module Main where

-- import Lib
import System.Environment
import System.Exit
import Control.Monad
import Args
import Data.Maybe
import Utils (intToStringBits, fillEmptyBits, getBit)

createEasyTest :: Args 
createEasyTest = Args {
    rule = Nothing,
    start = Nothing,
    Args.lines = Nothing, 
    window = Nothing,
    move = Nothing
}

readInt :: [Char] -> Maybe Int
readInt [] = Nothing
readInt string =
    case reads string of
    [(x, "")] -> Just x
    _ -> Nothing

type Cell = Char

data RuleCase = RuleCase {
    setup :: [Char],
    newCell :: Cell
}

data RuleDef = RuleDef {
    byte0 :: RuleCase,
    byte1 :: RuleCase,
    byte2 :: RuleCase,
    byte3 :: RuleCase,
    byte4 :: RuleCase,
    byte5 :: RuleCase,
    byte6 :: RuleCase,
    byte7 :: RuleCase
}

fst :: (a, b, c) -> a
fst (a, _, _) = a

snd :: (a, b, c) -> b
snd (_, b, _) = b

trd :: (a, b, c) -> c
trd (_, _, c) = c

createRuleCase :: Args -> Int -> RuleCase
createRuleCase args n = RuleCase {
    setup = lc:mc:rc:[],
    newCell = if getBit (7 - n) (fillEmptyBits (intToStringBits (fromJust (rule args))) 8) == '1' then '*' else ' '
} where
    lc = if getBit 0 (fillEmptyBits (intToStringBits n) 3) == '1' then '*' else ' '
    mc = if getBit 1 (fillEmptyBits (intToStringBits n) 3) == '1' then '*' else ' '
    rc = if getBit 2 (fillEmptyBits (intToStringBits n) 3) == '1' then '*' else ' '

createRule :: Args -> RuleDef
createRule args = RuleDef {
    byte0 = createRuleCase args 0,
    byte1 = createRuleCase args 1,
    byte2 = createRuleCase args 2,
    byte3 = createRuleCase args 3,
    byte4 = createRuleCase args 4,
    byte5 = createRuleCase args 5,
    byte6 = createRuleCase args 6,
    byte7 = createRuleCase args 7
}

parseArgs :: [String] -> Args -> Maybe Args
parseArgs ("--rule":y:zs) args = parseArgs zs args {rule = readInt y}
parseArgs ("--start":y:zs) args = parseArgs zs (args {start = readInt y})
parseArgs ("--lines":y:zs) args = parseArgs zs (args {Args.lines = readInt y})
parseArgs ("--window":y:zs) args = parseArgs zs (args {window = readInt y})
parseArgs ("--move":y:zs) args = parseArgs zs (args {move = readInt y})
parseArgs [] args = (Just args)
parseArgs _ _ = Nothing

verifArgs :: Maybe Args -> Maybe Args
verifArgs Nothing = Nothing
verifArgs (Just (Args Nothing s l w m)) = Nothing
verifArgs (Just (Args r s l w m)) = Just (Args r s' l' w' m')
    where
        s' = if isJust s then s else (Just 0)
        l' = if isJust l then l else Nothing
        w' = if isJust w then w else (Just 80)
        m' = if isJust m then m else (Just 0)

printLine :: [Char] -> Int -> IO()
printLine [] _ = return ()
printLine _ 0 = putChar '\n'
printLine (x:xs) n = if n <= 0 then return () else 
    putChar x >> printLine xs (n-1)

generateDefaultLine :: Int -> [Char]
generateDefaultLine n =
    take (div n 2) (cycle [' ']) ++ "*" ++ cycle[' ']

-- updateLineState :: [Char] -> Int -> RuleDef -> [Char]


stringToRuleComp :: [Char] -> [[Char]]
stringToRuleComp [] = []
stringToRuleComp (x:y:z:zs) = (x:y:[z]) : stringToRuleComp (y:z:zs)

ruleCompToNextState :: [[Char]] -> RuleDef -> [Char]
ruleCompToNextState [] _ = []
ruleCompToNextState (x:xs) rule = case x of
        "   " -> (newCell (byte0 rule)) : nextRuleComp
        "  *" -> (newCell (byte1 rule)) : nextRuleComp
        " * " -> (newCell (byte2 rule)) : nextRuleComp
        " **" -> (newCell (byte3 rule)) : nextRuleComp
        "*  " -> (newCell (byte4 rule)) : nextRuleComp
        "* *" -> (newCell (byte5 rule)) : nextRuleComp
        "** " -> (newCell (byte6 rule)) : nextRuleComp
        "***" -> (newCell (byte7 rule)) : nextRuleComp
    where nextRuleComp = ruleCompToNextState xs rule

invertRule :: RuleDef -> RuleDef
invertRule rule = RuleDef {
    byte0 = RuleCase {setup = (setup (byte0 rule)), newCell = (newCell (byte0 rule))},
    byte1 = RuleCase {setup = (setup (byte1 rule)), newCell = (newCell (byte4 rule))},
    byte2 = RuleCase {setup = (setup (byte2 rule)), newCell = (newCell (byte2 rule))},
    byte3 = RuleCase {setup = (setup (byte3 rule)), newCell = (newCell (byte6 rule))},
    byte4 = RuleCase {setup = (setup (byte4 rule)), newCell = (newCell (byte1 rule))},
    byte5 = RuleCase {setup = (setup (byte5 rule)), newCell = (newCell (byte5 rule))},
    byte6 = RuleCase {setup = (setup (byte6 rule)), newCell = (newCell (byte3 rule))},
    byte7 = RuleCase {setup = (setup (byte7 rule)), newCell = (newCell (byte7 rule))}
}


wolframEngine :: [Char] -> [Char] -> Args -> RuleDef -> Maybe Int -> IO()
wolframEngine _ _ _ _ (Just 0) = return ()
wolframEngine line beforeLine args rule (Just x) = do
    printLine (tail line) (fromJust (window args))
    wolframEngine ([neg] ++ (tail (ruleCompToNextState (stringToRuleComp (neg:( line))) rule))) beforeLine' args rule (Just (x-1))
    where
        neg = (head (ruleCompToNextState (stringToRuleComp ((head (tail line)):beforeLine)) (invertRule rule)))
        beforeLine' = ruleCompToNextState (stringToRuleComp ((head (tail line)):beforeLine)) (invertRule rule)
wolframEngine line beforeLine args rule Nothing = do
    printLine (tail line) (fromJust (window args))
    wolframEngine ([neg] ++ (tail (ruleCompToNextState (stringToRuleComp (neg:( line))) rule))) beforeLine' args rule Nothing
    where
        neg = (head (ruleCompToNextState (stringToRuleComp ((head (tail line)):beforeLine)) (invertRule rule)))
        beforeLine' = ruleCompToNextState (stringToRuleComp ((head (tail line)):beforeLine)) (invertRule rule)



main :: IO ()
main = do
    args <- getArgs
    parsedArgs <- return $ verifArgs $ parseArgs args createEasyTest
    case parsedArgs of
        Nothing -> putStrLn "Error : bad arguments"
            >> exitWith (ExitFailure 84)
        _ -> return ()
    wolframLine <- return $ generateDefaultLine
        (fromJust (window (fromJust parsedArgs)))
    wolframEngine (' ':wolframLine) (cycle [' ']) (fromJust parsedArgs) (createRule (fromJust parsedArgs)) (Args.lines (fromJust parsedArgs))