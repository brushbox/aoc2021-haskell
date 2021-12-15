module Day14
    ( part1
    , part2
    ) where

import Debug.Trace
-- import Data.Word
-- import qualified Data.ByteString as B
-- import qualified Data.Text.Encoding as E
-- import Data.Text (Text)
-- import qualified Data.Text as T
import Data.List
import Data.List.Split
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Template = String
type Element = Char
type PairMappings = Map (Element, Element) Element
type PairCounts = Map (Element, Element) Int
type ElementCounts = Map Element Int
type Polymer = (PairCounts, ElementCounts)

part1 :: IO ()
part1 = do
    putStrLn "Day 14 part 1"
    input <- readFile "day14.txt"
    -- input <- readFile "day14_example.txt"
    let (polymer, mapping) = parseInput input
    let result = calcPolymer mapping polymer 10
    putStrLn $ show result

part2 :: IO ()
part2 = do
    putStrLn "Day 14 part 2"
    input <- readFile "day14.txt"
    -- input <- readFile "day14_example.txt"
    let (polymer, mapping) = parseInput input
    let result = calcPolymer mapping polymer 40
    putStrLn $ show result

parseInput :: String -> (Polymer, PairMappings)
parseInput input = (polymer, pairMappings)
  where
    polymer = (pairCounts, elementCounts)
    pairCounts = countPairs template
    elementCounts = countElements template
    template = head ls
    pairMappings = foldl (\m (from, to) -> Map.insert from to m) Map.empty (parsedPairs)
    parsedPairs = map parsePair pairs
    pairs = drop 2 ls
    ls = lines input

countElements :: String -> ElementCounts
countElements template = foldl (\m c -> Map.insertWith (+) c 1 m) Map.empty template

countPairs :: String -> PairCounts
countPairs template = foldl (\m p -> Map.insertWith (+) p 1 m) Map.empty (zip template (tail template))

-- parseInput :: String -> (Template, PairMappings)
-- parseInput input = (template, pairMappings)
--   where
--     template = head ls
--     pairMappings = foldl (\m (from, to) -> Map.insert from to m) Map.empty (parsedPairs)
--     parsedPairs = map parsePair pairs
--     pairs = drop 2 ls
--     ls = lines input

parsePair :: String -> ((Element, Element), Element)
parsePair mapping = ((first, second), insert)
  where
    first = head from
    second = head $ tail from
    insert = head to
    [from, to] = splitOn " -> " mapping

-- initPairCounts :: Template -> PairCounts
-- initPairCounts template = traceShow pairCounts pairCounts
--   where
--     pairCounts = foldl (\counts pair -> Map.insertWith (+) pair 1 counts) Map.empty (zip template (tail template))

calcPolymer :: PairMappings -> Polymer -> Int -> Int
calcPolymer mappings polymer count = max - min
  where
    (min, max) = minMax elementCounts
    (_, elementCounts) = foldl (\polymer' c -> traceShow c (applyRules mappings polymer')) polymer [1..count]

-- calcPolymer :: PairMappings -> Polymer -> Int -> Int
-- calcPolymer mapping polymer count = max - min
--   where
--     (min, max) = minMax counts
--     counts = trace "counting ..." $ countLetters pairCounts'
--     pairCounts' = foldl (\pairCounts c -> trace (show c) (applyRules mapping pairCounts)) (initPairCounts template) [1..count]

applyRules :: PairMappings -> Polymer -> Polymer
applyRules mappings (pairs, elements) =
  -- Map.foldlWithKey (\(pairs', elements') pair@(a, b) count -> (updatePairs pairs' pair (inserted pair) count, updateElements elements' (inserted pair) count)) (Map.empty, elements) pairs
  Map.foldlWithKey (\polymer' pair count -> updatePolymer polymer' pair (inserted pair) count) (Map.empty, elements) pairs
  where
    inserted pair = mappings Map.! pair
    -- updatePairs m (a, b) i count = Map.insertWith (+) (a, i) count $ Map.insertWith (+) (i, b) count m
    -- updateElements es ch count = Map.insertWith (+) ch count es

updatePolymer :: Polymer -> (Element, Element) -> Element -> Int -> Polymer
updatePolymer (pairs, elements) (a, b) inserted count = (updatedPairs, updatedElements)
  where
    updatedPairs = Map.insertWith (+) (a, inserted) count $ Map.insertWith (+) (inserted, b) count pairs
    updatedElements = Map.insertWith (+) inserted count elements

-- countLetters :: PairCounts -> Map Element Int
-- countLetters pairCounts =
--    Map.foldlWithKey (\newMap (a, b) count -> updateCounts newMap a b count) Map.empty pairCounts
--    where
--      updateCounts m a b count =
--        Map.insertWith (+) a count $ Map.insertWith (+) b count m
-- --   trace ("Counting letters" ++ (show $ B.length polymer)) $ Map.fromListWith (+) $ (B.zipWith (\z _ -> (z, 1)) polymer polymer)
-- -- -- countLetters polymer =
-- -- --   trace ("Counting letters" ++ (show $ B.length polymer)) $ B.foldl (\m c -> Map.insertWith (+) c 1 m) Map.empty polymer

minMax :: ElementCounts -> (Int, Int)
minMax letterCounts = trace "getting minmax" (min, max)
  where
    min = head sorted
    max = last sorted
    sorted = sort $ Map.elems letterCounts
