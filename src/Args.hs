module Args
    (
        Args(..)
    ) where

import Data.Maybe

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

instance Show Args where
    show (Args r s l w m) = "rule: " ++ show r ++ "\n" ++
                            "start: " ++ show s ++ "\n" ++
                            "lines: " ++ show l ++ "\n" ++
                            "window: " ++ show w ++ "\n" ++
                            "move: " ++ show m ++ "\n"

