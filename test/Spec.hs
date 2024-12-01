{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Test.Hspec
import Test.QuickCheck
import Lib (parseLine, parseMultipleLines, sortLists, subtractPairs, 
           sumDifferences, processLists, getMultiSetOccurrences, getCardinalities)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.MultiSet as MS

-- Helper function to create a multiset from a list
fromList :: [Int] -> MS.MultiSet Int
fromList = MS.fromList

parseLineTests :: Spec
parseLineTests = describe "parseLine" $ do
    it "parses a simple space-separated string into integer pairs" $
        parseLine "123 456" `shouldBe` (123, 456)
        
    it "handles negative numbers" $
        parseLine "-42 789" `shouldBe` (-42, 789)
        
    -- QuickCheck properties
    it "converting pairs to string and back is identity" $ property $
        \(x :: Int) (y :: Int) -> parseLine (show x ++ " " ++ show y) == (x, y)
        
    it "first number in result matches first input" $ property $
        \(x :: Int) (y :: Int) -> fst (parseLine (show x ++ " " ++ show y)) == x
        
    it "second number in result matches second input" $ property $
        \(x :: Int) (y :: Int) -> snd (parseLine (show x ++ " " ++ show y)) == y

parseMultipleLinesTests :: Spec
parseMultipleLinesTests = describe "parseMultipleLines" $ do
    it "parses multiple lines of space-separated numbers" $ do
        let input = unlines ["1 2", "3 4", "5 6"]
        parseMultipleLines input `shouldBe` [(1, 2), (3, 4), (5, 6)]
        
    it "handles empty input" $
        parseMultipleLines "" `shouldBe` []
        
    it "handles single line input" $
        parseMultipleLines "7 8" `shouldBe` [(7, 8)]

sortListsTests :: Spec
sortListsTests = describe "sortLists" $ do
    it "sorts both lists in ascending order" $
        sortLists [3,1,2] [6,4,5] `shouldBe` ([1,2,3], [4,5,6] :: [Int])
        
    it "handles empty lists" $
        sortLists ([] :: [Int]) [] `shouldBe` ([], [])
        
    it "maintains list ordering property" $ property $
        \(xs :: [Int]) (ys :: [Int]) ->
            let (sortedXs, sortedYs) = sortLists xs ys
            in sort xs == sortedXs && sort ys == sortedYs

subtractPairsTests :: Spec
subtractPairsTests = describe "subtractPairs" $ do
    it "subtracts corresponding elements" $
        subtractPairs [1,2,3] [4,5,6] `shouldBe` [-3,-3,-3]
        
    it "handles empty lists" $
        subtractPairs [] [] `shouldBe` []
        
    it "handles lists with same elements" $
        subtractPairs [1,1,1] [1,1,1] `shouldBe` [0,0,0]
        
    it "maintains length" $ property $
        \(xs :: [Int]) ->
            let ys = xs in
            length (subtractPairs xs ys) == length xs

sumDifferencesTests :: Spec
sumDifferencesTests = describe "sumDifferences" $ do
    it "sums a list of differences" $
        sumDifferences [-3,-3,-3] `shouldBe` (-9)
        
    it "handles empty list" $
        sumDifferences [] `shouldBe` 0
        
    it "handles list with zeros" $
        sumDifferences [0,0,0] `shouldBe` 0
        
    it "sum is equal to regular sum" $ property $
        \(xs :: [Int]) -> sumDifferences xs == sum xs

processListsTests :: Spec
processListsTests = describe "processLists" $ do
    it "processes simple lists correctly" $ do
        let list1 = [3,1,2]
            ms = fromList [6,4,5]
        processLists list1 ms `shouldBe` 0  -- no matches
        
    it "handles empty inputs" $ do
        processLists [] (fromList []) `shouldBe` 0
        
    it "handles single element lists" $ do
        processLists [1] (fromList [2]) `shouldBe` 0
        
    it "result is independent of initial order" $ property $
        \(xs :: [Int]) ->
            let ms = fromList xs
            in processLists xs ms == processLists (reverse xs) ms
        
    it "result is always non-negative" $ property $
        \(xs :: [Int]) ->
            not (null xs) ==>
            let ys = map negate xs
                ms = fromList ys
            in processLists xs ms >= 0

multisetTests :: Spec
multisetTests = describe "Multiset operations" $ do
    describe "getMultiSetOccurrences" $ do
        it "counts occurrences in a simple multiset" $ do
            let ms = fromList [1,1,2,3,3,3]
            getMultiSetOccurrences ms `shouldMatchList` [(1,2), (2,1), (3,3)]
            
        it "handles empty multiset" $ do
            getMultiSetOccurrences (fromList []) `shouldBe` []
            
        it "handles single element" $ do
            getMultiSetOccurrences (fromList [1]) `shouldBe` [(1,1)]
            
    describe "getCardinalities" $ do
        it "gets correct cardinalities for present elements" $ do
            let ms = fromList [1,1,2,3,3,3]
            getCardinalities [1,2,3] ms `shouldBe` [2,1,3]
            
        it "returns zero for missing elements" $ do
            let ms = fromList [1,1,2]
            getCardinalities [3,4,5] ms `shouldBe` [0,0,0]
            
        it "handles mixed present and missing elements" $ do
            let ms = fromList [1,1,2,3,3]
            getCardinalities [1,4,3,5] ms `shouldBe` [2,0,2,0]
            
        it "handles empty input list" $ do
            let ms = fromList [1,2,3]
            getCardinalities [] ms `shouldBe` []
            
        it "handles empty multiset" $ do
            let ms = fromList []
            getCardinalities [1,2,3] ms `shouldBe` [0,0,0]
            
        -- Property: cardinality is always non-negative
        it "always returns non-negative cardinalities" $ property $
            \(xs :: [Int]) (ys :: [Int]) ->
                let ms = fromList ys
                    cards = getCardinalities xs ms
                in all (>= 0) cards
                
        -- Property: cardinality matches frequency in original list
        it "cardinality matches frequency in list" $ property $
            \(xs :: [Int]) ->
                not (null xs) ==>
                let ms = fromList xs
                    x = head xs
                    expected = length $ filter (== x) xs
                in MS.occur x ms == expected

    describe "processLists with multiset" $ do
        it "multiplies values with cardinalities correctly" $ do
            let list1 = [1,2,3]  -- values
                ms = fromList [1,1,2,3,3,3]  -- 1 appears 2x, 2 appears 1x, 3 appears 3x
            processLists list1 ms `shouldBe` (1*2 + 2*1 + 3*3)  -- 1*2 + 2*1 + 3*3 = 13
            
        it "handles empty inputs" $ do
            processLists [] (fromList []) `shouldBe` 0
            
        it "handles no matches" $ do
            let list1 = [1,2,3]
                ms = fromList [4,5,6]
            processLists list1 ms `shouldBe` 0
            
        it "handles all matches with single occurrences" $ do
            let list1 = [1,2,3]
                ms = fromList [1,2,3]
            processLists list1 ms `shouldBe` (1*1 + 2*1 + 3*1)  -- each value appears once
            
        -- Property: result is zero if no matches
        it "returns zero for disjoint sets" $ property $
            \(xs :: [Int]) ->
                let ys = map (+10000) xs  -- ensure no overlap
                    ms = fromList ys
                in processLists xs ms == 0
                
        -- Property: result matches simple calculation for single number
        it "correctly processes single number with multiple occurrences" $ property $
            \(x :: Int) (n :: Int) ->
                n >= 0 ==>
                let ms = fromList (replicate n x)
                in processLists [x] ms == x * n

main :: IO ()
main = hspec $ do
    parseLineTests
    parseMultipleLinesTests
    sortListsTests
    subtractPairsTests
    sumDifferencesTests
    processListsTests
    multisetTests
