module Utils
    (
        intToStringBits,
        fillEmptyBits,
        getBit,
        Utils.readInt,
        printLine,
        strToCase
    ) where

import Numeric
import Data.Char

-- | Converts an Int to an array of its bits
intToStringBits :: Int -> [Char]
intToStringBits n = showIntAtBase 2 intToDigit n ""

fillEmptyBits :: [Char] -> Int -> [Char]
fillEmptyBits [] x = fillEmptyBits ['0'] x
fillEmptyBits (x:xs) n = if length (x:xs) < n
    then fillEmptyBits ('0':(x:xs)) n else x:xs

getBit :: Int -> [Char] -> Char
getBit _ [] = '0'
getBit n (x:xs) = if n == 0 then x else getBit (n-1) xs

readInt :: [Char] -> Maybe Int
readInt [] = Nothing
readInt string =
    case reads string of
        [(x, "")] -> Just x
        _ -> Nothing

printLine :: Int -> [Char] -> IO ()
printLine _ [] = return ()
printLine 0 _ = putChar '\n'
printLine n line = putStrLn (take n line)

strToCase :: [Char] -> [[Char]]
strToCase [] = []
strToCase (x : y : z : zs) =
    (x : y : [z]) :
    strToCase (y : z : zs)
strToCase _ = []