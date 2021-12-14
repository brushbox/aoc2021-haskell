module Day14
    ( part1
    , part2
    ) where

import Data.List
import Data.List.Split
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Template = String
type PairMappings = Map (Char, Char) Char

part1 :: IO ()
part1 = do
    putStrLn "Day 14 part 1"
    input <- readFile "day14.txt"
    -- input <- readFile "day14_example.txt"
    let (template, mapping) = parseInput input
    let result = calcPolymer mapping template 10
    putStrLn $ show result

part2 :: IO ()
part2 = do
    putStrLn "Day 14 part 2"
    input <- readFile "day14.txt"
    -- input <- readFile "day14_example.txt"
    let (template, mapping) = parseInput input
    let result = calcPolymer mapping template 40
    putStrLn $ show result

calcPolymer :: PairMappings -> Template -> Int -> Int
calcPolymer mapping template count = max - min
  where
    (min, max) = minMax counts
    counts = countLetters t'
    t' = foldl (\t _ -> applyRules mapping t) template [1..count]
  
parseInput :: String -> (Template, PairMappings)
parseInput input = (template, pairMappings)
  where
    template = head ls
    pairMappings = foldl (\m (from, to) -> Map.insert from to m) Map.empty (parsedPairs)
    parsedPairs = map parsePair pairs
    pairs = drop 2 ls
    ls = lines input

parsePair :: String -> ((Char, Char), Char)
parsePair mapping = ((first, second), insert)
  where
    [first, second] = from
    insert = head to
    [from, to] = splitOn " -> " mapping

applyRules :: PairMappings -> Template -> Template
applyRules mappings template = template'
  where
    template' = (head template):inserted
    inserted = foldr (\pair template' -> prependMapping pair template') "" pairs
    pairs = zip template (drop 1 template)
    prependMapping pair template =
      let [a, b] = applyMapping pair in
          (a:b:template)
    applyMapping pair@(_,b) = [mappings Map.! pair, b]

countLetters :: String -> Map Char Int
countLetters polymer =
  foldl (\m c -> Map.insertWith (+) c 1 m) Map.empty polymer

minMax :: Map Char Int -> (Int, Int)
minMax letterCounts = (min, max)
  where
    min = snd $ head sorted
    max = snd $ last sorted
    sorted = sortBy (\(_, a) (_, b) -> compare a b) $ Map.toList letterCounts