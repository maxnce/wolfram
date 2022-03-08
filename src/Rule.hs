module Rule
    (
    Cell, RuleCase, RuleDef, createRuleCase, createRule, invertRule, computeState
    ) where

import Args
import Utils (intToStringBits, fillEmptyBits, getBit)
import System.Exit (ExitCode (ExitFailure), exitWith)
import Data.Maybe (fromJust)

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

data Ruleb = Ruleb Int Int Int Int Int Int Int Int
createRuleb :: Ruleb
createRuleb = Ruleb 0 0 0 0 0 0 0 0