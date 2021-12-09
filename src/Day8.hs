module Day8
    ( part1
    , part2
    ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe

import Data.List
import Data.List.Split

type Signals = Set Char
type DigitMapping = Map Signals Int

part1 :: IO ()
part1 = do
    putStrLn "Day 8 part 1"
    contents <- readFile "day8.txt"
    let answer = sum $ map (length . uniqueSegments . snd . parseLine) $ lines contents
    putStrLn $ show answer

part2 :: IO ()
part2 = do
    putStrLn "Day 8 part 2"
    contents <- readFile "day8.txt"
    let displays = map parseLine $ lines contents
    let results = map (\(ins, outs) -> mappingToNumber outs (inferDigitMappings ins)) displays
    putStrLn $ show (sum results)

uniqueSegments :: [String] -> [String]
uniqueSegments inputs =
    filter lengthMatches inputs
    where
        lengthMatches input = case (length input) of
            2 -> True
            3 -> True
            4 -> True
            7 -> True
            otherwise -> False

parseLine :: String -> ([String], [String])
parseLine line =
    (splitOn " " diagnostics, splitOn " " display)
    where
        [diagnostics, display] = splitOn " | "  line

mappingToNumber :: [String] -> DigitMapping -> Int
mappingToNumber inputs mapping =
    foldl (\num signals -> num * 10 + (digit signals)) 0 inputs
    where
        digit :: String -> Int
        digit signals = Map.findWithDefault (-1) (Set.fromList signals) mapping

inferDigitMappings :: [String] -> DigitMapping
inferDigitMappings unknownDigits =
    foldl (\m (signals, val) -> insert signals val m) Map.empty (zip [zero, one, two, three, four, five, six, seven, eight, nine] [0..])
    where
        insert :: String -> Int -> DigitMapping -> DigitMapping
        insert signals value mp = Map.insert (Set.fromList signals) value mp

        zero  = findWhere (notIn [six, nine]) sixSegments
        one   = findWhere (lengthIs 2) unknownDigits
        two   = findWhere (notIn [three, five]) fiveSegments
        three = findWhere (isSubset one) fiveSegments
        four  = findWhere (lengthIs 4) unknownDigits
        five  = findWhere (isSuperset six) fiveSegments
        six   = findWhere (isNotSubset one) sixSegments
        seven = findWhere (lengthIs 3) unknownDigits
        eight = findWhere (lengthIs 7) unknownDigits
        nine  = findWhere (isSubset three) sixSegments

        fiveSegments = filterByLength 5 unknownDigits
        sixSegments = filterByLength 6 unknownDigits

findWhere :: (String -> Bool) -> [String] -> String
findWhere pred candidates =
    case (find pred candidates) of
        Nothing -> error "We did something wrong"
        Just result -> result

filterByLength :: Int -> [String] -> [String]
filterByLength len = filter (lengthIs len)

notIn :: [String] -> String -> Bool
notIn candidates = not . (`elem` candidates)

lengthIs :: Int -> String -> Bool
lengthIs len = (len ==) . length

isSubset :: String -> String -> Bool
isSubset lookFor within =
    (Set.intersection lookForSet withinSet) == lookForSet
    where
        lookForSet = Set.fromList lookFor
        withinSet = Set.fromList within

isNotSubset :: String -> String -> Bool
isNotSubset lookFor = not . (isSubset lookFor)

isSuperset :: String -> String -> Bool
isSuperset super = (`isSubset` super)