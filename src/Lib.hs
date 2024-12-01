module Lib
    ( someFunc
    , parseLine
    , parseMultipleLines
    , processLists
    , sortLists
    , subtractPairs
    , sumDifferences
    , getMultiSetOccurrences
    , getCardinalities
    ) where

import System.IO
import Data.List (sort)
import qualified Data.MultiSet as MS

-- Parse a line into a tuple of two integers
parseLine :: String -> (Int, Int)
parseLine line = 
    case map read $ words line of
        [a, b] -> (a, b)
        _ -> error "Invalid input line format: expected exactly two numbers"

-- Parse multiple lines into a list of integer pairs
parseMultipleLines :: String -> [(Int, Int)]
parseMultipleLines input = map parseLine (lines input)

-- Sort two lists
sortLists :: (Ord a) => [a] -> [a] -> ([a], [a])
sortLists xs ys = (sort xs, sort ys)

-- Get occurrences of each number in the multiset
getMultiSetOccurrences :: MS.MultiSet Int -> [(Int, Int)]
getMultiSetOccurrences ms = 
    [(x, MS.occur x ms) | x <- MS.distinctElems ms]

-- Get cardinalities for a list of numbers from a multiset
getCardinalities :: [Int] -> MS.MultiSet Int -> [Int]
getCardinalities xs ms = map (`MS.occur` ms) xs

-- Subtract corresponding elements
subtractPairs :: [Int] -> [Int] -> [Int]
subtractPairs xs ys 
    | length xs == length ys = zipWith (-) xs ys
    | otherwise = error "Lists must be of equal length"

-- Sum up the differences
sumDifferences :: [Int] -> Int
sumDifferences = sum

-- Process two lists completely
processLists :: [Int] -> MS.MultiSet Int -> Int
processLists xs ms = 
    let pairs = zip (sort xs) (getCardinalities (sort xs) ms)
    in sum [x * card | (x, card) <- pairs]

-- Read file and return two lists of integers
someFunc :: IO ()
someFunc = do
    content <- readFile "test/input.txt"
    let pairs = parseMultipleLines content
        list1 = map fst pairs
        list2 = map snd pairs
        multiset2 = MS.fromList list2
    putStrLn "First list (first 5):"
    print $ take 5 list1
    putStrLn "\nCardinalities for first 5 numbers:"
    print $ take 5 $ getCardinalities list1 multiset2
    putStrLn "\nTotal sum of cardinalities:"
    print $ processLists list1 multiset2
