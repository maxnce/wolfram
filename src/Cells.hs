module Cells
    (
        Cell,
        Generation,
    ) where

import Data.Maybe

-- Data type for each cell

data Cell = Alive | Dead
    deriving (Eq)

instance Show Cell where
    show Alive = "*"
    show Dead = " "

-- Data type for a generation

type Generation = [Cell]
