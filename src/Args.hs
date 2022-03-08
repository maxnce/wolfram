module Args
    (
        Args(..), verifArgs, parseArgs, createTemplateArgs
    ) where

import Data.Maybe
import Utils (readInt)

-- | Arguments for the program.

type Rule = (Maybe Int)
type Start = (Maybe Int)
type Lines = (Maybe Int)
type Window = (Maybe Int)
type Move = (Maybe Int)

data Args = Args {
    rule :: Rule,
    start :: Start,
    lines :: Lines,
    window :: Window,
    move :: Move
}

verifArgs :: Maybe Args -> Maybe Args
verifArgs Nothing = Nothing
verifArgs (Just (Args Nothing s l w m)) = Nothing
verifArgs (Just (Args r s l w m)) = Just (Args r s' l' w' m')
    where
        s' = if isJust s then s else Just 0
        l' = if isJust l then l else Nothing
        w' = if isJust w then w else Just 80
        m' = if isJust m then m else Just 0

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

createTemplateArgs :: Args
createTemplateArgs = Args {
    rule = Nothing,
    start = Nothing,
    Args.lines = Nothing,
    window = Nothing,
    move = Nothing}


instance Show Args where
    show (Args r s l w m) = "rule: " ++ show r ++ "\n" ++
                            "start: " ++ show s ++ "\n" ++
                            "lines: " ++ show l ++ "\n" ++
                            "window: " ++ show w ++ "\n" ++
                            "move: " ++ show m ++ "\n"

