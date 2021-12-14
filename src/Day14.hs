module Day14
    ( part1
    , part2
    ) where

import Debug.Trace
import Data.Word
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as E
import Data.Text (Text)
import qualified Data.Text as T
import Data.List
import Data.List.Split
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Template = B.ByteString
type Element = Word8
type PairMappings = Map (Element, Element) Element

part1 :: IO ()
part1 = do
    putStrLn "Day 14 part 1"
    input <- E.decodeUtf8 <$> B.readFile "day14.txt"
    -- input <- readFile "day14_example.txt"
    let (template, mapping) = parseInput input
    let t' = applyRules mapping template
    let result = calcPolymer mapping template 10
    putStrLn $ show result
    -- putStrLn $ show t'

part2 :: IO ()
part2 = do
    putStrLn "Day 14 part 2"
    -- input <- E.decodeUtf8 <$> B.readFile "day14.txt"
    input <- E.decodeUtf8 <$> B.readFile "day14_example.txt"
    let (template, mapping) = parseInput input
    let result = calcPolymer mapping template 20
    putStrLn $ show result
    -- putStrLn $ show template

calcPolymer :: PairMappings -> Template -> Int -> Int
calcPolymer mapping template count = max - min
  where
    (min, max) = minMax counts
    counts = trace "counting ..." $ countLetters t'
    t' = foldl (\t c -> trace (show c) (applyRules mapping t)) template [1..count]

parseInput :: Text -> (Template, PairMappings)
parseInput input = (template, pairMappings)
  where
    template = E.encodeUtf8 $ head ls
    pairMappings = foldl (\m (from, to) -> Map.insert from to m) Map.empty (parsedPairs)
    parsedPairs = map parsePair pairs
    pairs = drop 2 ls
    ls = T.lines input

parsePair :: Text -> ((Element, Element), Element)
parsePair mapping = ((first, second), insert)
  where
    first = B.head (E.encodeUtf8 from)
    second = B.head $ B.tail (E.encodeUtf8 from)
    insert = B.head (E.encodeUtf8 to)
    [from, to] = T.splitOn (T.pack " -> ") mapping

applyRules :: PairMappings -> Template -> Template
applyRules mappings template = template'
  where
    template' = (B.head template) `B.cons` inserted
    inserted = foldr (\pair template' -> prependMapping pair template') B.empty pairs
    pairs = B.zip template (B.drop 1 template)
    prependMapping pair template =
      let (a, b) = applyMapping pair in
        a `B.cons` (b `B.cons` template)
    applyMapping pair@(_,b) = (mappings Map.! pair, b)

countLetters :: B.ByteString -> Map Element Int
countLetters polymer =
  trace ("Counting letters" ++ (show $ B.length polymer)) $ Map.fromListWith (+) $ (B.zipWith (\z _ -> (z, 1)) polymer polymer)
-- countLetters polymer =
--   trace ("Counting letters" ++ (show $ B.length polymer)) $ B.foldl (\m c -> Map.insertWith (+) c 1 m) Map.empty polymer

minMax :: Map Element Int -> (Int, Int)
minMax letterCounts = trace "getting minmax" (min, max)
  where
    min = head sorted
    max = last sorted
    sorted = sort $ Map.elems letterCounts
