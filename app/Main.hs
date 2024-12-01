module Main where

import Lib
import qualified Data.Set as Set
import qualified Data.MultiSet as MS
import Data.List (sortOn)
import Data.Ord (Down(..))

main :: IO ()
main = do
    content <- readFile "test/input.txt"
    let pairs = parseMultipleLines content
        list1 = map fst pairs
        list2 = map snd pairs
        set1 = Set.fromList list1
        multiset2 = MS.fromList list2
        
    putStrLn "Analysis of the data:"
    putStrLn $ "First list length: " ++ show (length list1)
    putStrLn $ "Unique numbers in first list: " ++ show (Set.size set1)
    putStrLn $ "Total elements in multiset: " ++ show (MS.size multiset2)
    putStrLn $ "Distinct elements in multiset: " ++ show (MS.distinctSize multiset2)
    
    let cardinalities = zip list1 (getCardinalities list1 multiset2)
        topCardinalities = take 5 $ sortOn (Down . snd) cardinalities
        
    putStrLn "\nTop 5 numbers from first list by their cardinality in the multiset:"
    mapM_ (\(num, card) -> putStrLn $ show num ++ " appears " ++ show card ++ " time(s)")
        topCardinalities
        
    putStrLn "\nNumbers from first list with zero occurrences in multiset:"
    let zerosCount = length $ filter ((==0) . snd) cardinalities
    putStrLn $ show zerosCount ++ " numbers have zero occurrences"
    
    let totalCardinality = processLists list1 multiset2
    putStrLn $ "\nSum of all cardinalities: " ++ show totalCardinality
