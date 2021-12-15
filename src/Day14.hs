module Day14
    ( part1
    , part2
    ) where

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
    pairMappings = foldl (\m (from, to) -> Map.insert from to m) Map.empty parsedPairs
    parsedPairs = map parsePair pairs
    pairs = drop 2 ls
    ls = lines input

countElements :: String -> ElementCounts
countElements template = foldl (\m c -> Map.insertWith (+) c 1 m) Map.empty template

countPairs :: String -> PairCounts
countPairs template = foldl (\m p -> Map.insertWith (+) p 1 m) Map.empty (zip template (tail template))

parsePair :: String -> ((Element, Element), Element)
parsePair mapping = ((first, second), insert)
  where
    [first, second] = from
    [from, [insert]] = splitOn " -> " mapping

calcPolymer :: PairMappings -> Polymer -> Int -> Int
calcPolymer mappings polymer count = max - min
  where
    (min, max) = minMax elementCounts
    (_, elementCounts) = foldl (\polymer' _ -> applyRules mappings polymer') polymer [1..count]

applyRules :: PairMappings -> Polymer -> Polymer
applyRules mappings (pairs, elements) =
  Map.foldlWithKey (\polymer' pair count -> updatePolymer polymer' pair (inserted pair) count) (Map.empty, elements) pairs
  where
    inserted pair = mappings Map.! pair

updatePolymer :: Polymer -> (Element, Element) -> Element -> Int -> Polymer
updatePolymer (pairs, elements) (a, b) inserted count = (updatedPairs, updatedElements)
  where
    updatedPairs = Map.insertWith (+) (a, inserted) count $ Map.insertWith (+) (inserted, b) count pairs
    updatedElements = Map.insertWith (+) inserted count elements

minMax :: ElementCounts -> (Int, Int)
minMax letterCounts = (minV, maxV)
  where
    counts = Map.elems letterCounts
    (minV, maxV) = foldl (\(mi, ma) v -> (min mi v, max ma v)) (maxBound, 0) counts
