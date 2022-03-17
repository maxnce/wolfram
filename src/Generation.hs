module Generation
    (
        getGenerationTable
    )where
import Rule (Cell, RuleDef, createRule, computeState, invertRule)
import Args
import Data.Maybe (fromJust, isNothing, isJust)
import Utils (strToCase)


getFirstCharFromBLine :: [Cell] -> [Cell] -> RuleDef  -> Cell
getFirstCharFromBLine line beforeLine rule =
    head (computeState
        (strToCase (head (tail line) : beforeLine)) (invertRule rule))

createInitialTuple :: [Cell] -> Args -> ([Cell], [Cell])
createInitialTuple wLine args =
    (if fromJust (move args) > 0 then replicate (
    fromJust (move args)) ' ' ++ wLine else drop
    (abs (fromJust (move args))) wLine, if fromJust
    (move args) > 0 then repeat ' ' else reverse (take
    (abs (fromJust (move args))) wLine) ++ repeat ' ')


getGenerationTable :: Args -> [[Cell]]
getGenerationTable args =
    uncurry wolframEngine zTuple args wRule wGen
    where
        wLine = generateDefaultLine (fromJust (window args))
        wRule = createRule args
        zTuple = createInitialTuple wLine args
        wGen = if isNothing (Args.lines args) then Nothing
            else Just (fromJust (start args) +
            fromJust (Args.lines args))

wolframEngine :: [Cell] -> [Cell] -> Args -> RuleDef -> Maybe Int -> [[Cell]]
wolframEngine _ _ _ _ (Just 0) = []
wolframEngine line beforeLine args rule n =
    line : wolframEngine currString newBLine' args rule nMinusOne
    where
    neg = getFirstCharFromBLine line beforeLine rule
    newBLine' = computeState (strToCase (head (tail line) : beforeLine))
        (invertRule rule)
    currString =  neg : tail (computeState (strToCase (neg : line)) rule)
    nMinusOne = if isJust n then Just (fromJust n - 1) else Nothing

generateDefaultLine :: Int -> [Cell]
generateDefaultLine n = replicate (div n 2) ' ' ++ "*" ++ repeat ' '