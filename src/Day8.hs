module Day8
    ( part1
    , part2
    ) where

import Debug.Trace
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe

import Data.List
import Data.List.Split

type Inputs = [String]
type Outputs = [String]

data Segment = A | B | C | D | E | F | G
    deriving (Enum, Show, Ord, Eq)

part1 :: IO ()
part1 = do
    putStrLn "Day 8 part 1"
    contents <- readFile "day8.txt"
    let displays = map parseLine $ lines contents
    let displays' = map (\(i, o) -> (uniqueSegments i, o)) displays
    let outputs = map (\(_, o) -> uniqueSegments o) displays
    let answer = sum $ map length outputs

    putStrLn $ show answer

uniqueSegments :: Inputs -> Inputs
uniqueSegments inputs =
    filter lengthMatches inputs
    where
        lengthMatches input = case (length input) of
            2 -> True
            3 -> True
            4 -> True
            7 -> True
            otherwise -> False

parseLine :: String -> (Inputs, Outputs)
parseLine line =
    (inputs, outputs)
    where
        inputs = map sort $ splitOn " " diagnostics
        outputs = map sort $ splitOn " " display
        [diagnostics, display] = splitOn " | "  line

part2 :: IO ()
part2 = do
    putStrLn "Day 8 part 2"
    -- contents <- readFile "day8_example.txt"
    contents <- readFile "day8.txt"
    let displays = map parseLine $ lines contents
    -- let inputs = ["be", "cfbegad", "cbdgef", "fgaecd", "cgeb", "fdcge", "agebfd", "fecdb", "fabcd", "edb"]
    -- let outputs = ["fdgacbe", "cefdb", "cefbgd", "gcbe"]
    -- let mapping = inferDigitMappings inputs
    -- let num = mappingToNumber outputs mapping
    -- putStrLn $ show mapping
    -- putStrLn $ show num

    -- let inputs2 = ["edbfga", "begcd", "cbg", "gc", "gcadebf", "fbgde", "acbgfd", "abcde", "gfcbed", "gfec"]
    -- let outputs2 = ["fcgedb", "cgb", "dgebacf", "gc"]
    -- let mapping2 = inferDigitMappings inputs2
    -- let num2 = mappingToNumber outputs2 mapping2
    -- putStrLn $ show mapping2
    -- putStrLn $ show num2
    let results = map (\(ins, outs) -> mappingToNumber outs (inferDigitMappings ins)) displays
    putStrLn $ show results
    putStrLn $ show (sum results)

type Input = Char
type Mapping = (Char, Set Segment )
type Signals = Set Char

type DigitMapping = Map Signals Int

mappingToNumber :: [String] -> DigitMapping -> Int
mappingToNumber inputs mapping =
    foldl (\num signals -> num * 10 + (digit signals)) 0 inputs
    where
        digit :: String -> Int
        -- digit signals = Map.findWithDefault -1 signals mapping
        digit signals = fromMaybe (-1) $ Map.lookup (Set.fromList signals) mapping

inferDigitMappings :: [String] -> DigitMapping
inferDigitMappings unknownDigits =
    mapTwo
    where
        mapOne = insert one 1 Map.empty
        mapFour = insert four 4 mapOne
        mapSeven = insert seven 7 mapFour
        mapEight = insert eight 8 mapSeven
        mapThree = insert three 3 mapEight
        mapSix = insert six 6 mapThree
        mapNine = insert nine 9 mapSix
        mapZero = insert zero 0 mapNine
        mapFive = insert five 5 mapZero
        mapTwo = insert two 2 mapFive

        insert :: String -> Int -> DigitMapping -> DigitMapping
        insert signals value mp = Map.insert (Set.fromList signals) value mp
        one :: String
        one = findByLength 2
        four = findByLength 4
        seven = findByLength 3
        eight = findByLength 7
        three = findBySubset one fives
        six = findByNotSubset one sixes
        nine = findBySubset three sixes
        -- zero = fromMaybe "" $ find (\s -> not (s `elem` [six, nine])) sixes
        zero = fromMaybe "" $ find (not . (`elem` [six, nine])) sixes
        five = findBySuperset six fives
        two = fromMaybe "" $ find (not . (`elem` [three, five])) fives

        fives :: [String]
        fives = (filterByLength 5 unknownDigits)
        sixes :: [String]
        sixes = filterByLength 6 unknownDigits

        findByLength :: Int -> String
        findByLength len = fromMaybe "" $ find (\s -> length s == len) unknownDigits
        findBySubset :: String -> [String] -> String
        findBySubset lookFor candidates = fromMaybe "" $ find (isSubset lookFor) candidates
        findBySuperset :: String -> [String] -> String
        findBySuperset super candidates = fromMaybe "" $ find (\lookFor -> isSubset lookFor super) candidates
        findByNotSubset :: String -> [String] -> String
        findByNotSubset lookFor candidates = fromMaybe "" $ find (not . (isSubset lookFor)) candidates
        isSubset :: String -> String -> Bool
        isSubset lookFor within =
            (Set.intersection lookForSet withinSet) == lookForSet
            where
                lookForSet = Set.fromList lookFor
                withinSet = Set.fromList within

filterByLength :: Int -> [String] -> [String]
-- filterByLength len strs = filter (\s -> length s == len) strs
filterByLength len strs = filter ((len ==) . length) strs

intersectMappings :: [Mapping] -> [Mapping] -> [Mapping]
intersectMappings a b =
    map intersect $ zip a b
    where
        intersect :: (Mapping, Mapping) -> Mapping
        -- intersect ((c, m1), (_, m2)) = traceShow (m1, m2) (c, Set.intersection m1 m2)
        intersect ((c, m1), (_, m2)) = (c, Set.intersection m1 m2)

initialMappings :: [Mapping]
initialMappings = map (\c -> (c, allSegments)) "abcdefg"

calcMappings :: [Char] -> [Mapping]
calcMappings inputs =
    map calcMapping "abcdefg"
    where
        calcMapping c
            | c `elem` inputs = (c, candidates inputs)
            | otherwise = (c, inverseCandidates inputs)
    
-- invertInput :: [Char] -> [Char]
-- invertInput input = Set.toList $ Set.difference (Set.fromList "abcdefg") (Set.fromList input)

candidates :: [Char] -> Set Segment
candidates inputs = 
    case (length inputs) of
        1 -> set2
        3 -> set3
        4 -> set4
        otherwise -> allSegments

inverseCandidates :: [Char] -> Set Segment
inverseCandidates inputs = 
    case (length inputs) of
        2 -> invertSet set2
        3 -> invertSet set3
        4 -> invertSet set4
        5 -> Set.fromList [B, C, E, F]
        6 -> Set.fromList [C, D, E]
        otherwise -> Set.empty

set2 :: Set Segment
set2 = Set.fromList [C, F]

set3 :: Set Segment
set3 = Set.fromList [A, C, F]

set4 :: Set Segment
set4 = Set.fromList [B, C, D, F]

invertSet :: Set Segment -> Set Segment
invertSet = Set.difference allSegments

allSegments :: Set Segment
allSegments = Set.fromList [A, B, C, D, E, F, G]

