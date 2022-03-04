module Rule
    (
    RuleDef,
    -- printResult,
    -- createRule
    ) where

import Args
import Utils (intToStringBits, fillEmptyBits, getBit)
import System.Exit (ExitCode (ExitFailure), exitWith)

data Cell = Alive | Dead
    deriving (Eq)

instance Show Cell where
    show Alive = "*"
    show Dead = " "


data RuleCase = RuleCase {
    left :: Cell,
    mid :: Cell,
    right :: Cell,
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

-- createRuleCase :: Args -> Int -> RuleCase
-- createRuleCase args n = RuleCase {
--     left = if getBit 0 (fillEmptyBits (intToStringBits n) 3) == '1' then Alive else Dead,
--     mid = if getBit 1 (fillEmptyBits (intToStringBits n) 3) == '1' then Alive else Dead,
--     right = if getBit 2 (fillEmptyBits (intToStringBits n) 3) == '1' then Alive else Dead,
--     newCell = if getBit n (fillEmptyBits (intToStringBits (rule args)) 8) == '1' then Alive else Dead
-- }

-- createRule :: Args -> RuleDef
-- createRule args = RuleDef {
--     byte0 = createRuleCase args 0,
--     byte1 = createRuleCase args 1,
--     byte2 = createRuleCase args 2,
--     byte3 = createRuleCase args 3,
--     byte4 = createRuleCase args 4,
--     byte5 = createRuleCase args 5,
--     byte6 = createRuleCase args 6,
--     byte7 = createRuleCase args 7
-- }

-- printLine :: Args -> RuleDef -> [Cell] -> IO ()
-- printLine args rule line = do
--     putStrLn $ concatMap show line
--     if rule args == 255 then
--         exitWith ExitFailure
--     else
--         return ()

-- printResult :: Args -> RuleDef -> [Cell] -> IO ()
-- printResult args rule = do
--     printLine args rule
--     updateLine args rule
--     exitWith (ExitFailure 1)
