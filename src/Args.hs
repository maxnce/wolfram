module Args
    (
        Args(..)
    ) where

import Data.Maybe

-- | Arguments for the program.

type Rule = Int
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

