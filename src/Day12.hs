module Day12
    ( part1
    , part2
    ) where

import Debug.Trace
import Data.Char
import Data.Maybe
import Data.List.Split
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Cave = Start | End | Small String | Big String
    deriving (Eq, Ord, Show)
type CaveMap = Map Cave [Cave]
type Path = [Cave]

-- input = "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end"
-- input = "dc-end\nHN-start\nstart-kj\ndc-start\ndc-HN\nLN-dc\nHN-end\nkj-sa\nkj-HN\nkj-dc"

part1 :: IO ()
part1 = do
    putStrLn "Day 12 part 1"
    input <- readFile "day12.txt"
    let caves = parseInput input
    let caveMap = buildCaveMap caves
    let paths = countPaths caveMap [] Start
    putStrLn $ show paths

part2 :: IO ()
part2 = do
    putStrLn "Day 12 part 2"
    input <- readFile "day12.txt"
    let caves = parseInput input
    let caveMap = buildCaveMap caves
    let paths = countPaths2 caveMap [] (False, Start)
    putStrLn $ show paths

parseInput :: String -> [(Cave, Cave)]
parseInput s = map parseLine ls
    where
        ls = lines s

parseLine :: String -> (Cave, Cave)
parseLine l = ((strToCave a), (strToCave b))
    where
        [a, b] = splitOn "-"  l

strToCave :: String -> Cave
strToCave s | s == "start" = Start
            | s == "end" = End
            | all isUpper s = Big s
            | otherwise = Small s

buildCaveMap :: [(Cave, Cave)] -> CaveMap
buildCaveMap edges =
    foldl (\m (a, b) -> insertBiDirectionalEdge a b m) Map.empty edges
    where
        insertBiDirectionalEdge a b m =
            Map.insertWith insertEdge a [b] (Map.insertWith insertEdge b [a] m)
        insertEdge [edge] edges = edge:edges

countPaths :: CaveMap -> Path -> Cave -> Int
countPaths  _ _ End  = 1
countPaths m path cave = sum $ map (countPaths m (cave:path)) validEdges
    where
        edges = m Map.! cave
        validEdges = filter (\c -> canVisit c path) edges
        canVisit :: Cave -> Path -> Bool
        canVisit (Big _) _ = True
        canVisit cave path = cave `notElem` path

countPaths2 :: CaveMap -> Path -> (Bool, Cave) -> Int
countPaths2 _ _ (_, End) = 1
countPaths2 caveMap path (twice,cave) = sum $ map (countPaths2 caveMap (cave:path)) validEdges
    where
        edges = caveMap Map.! cave
        validEdges = mapMaybe (canVisit path) (zip (repeat twice) edges)
        canVisit :: Path -> (Bool,Cave) -> Maybe (Bool, Cave)
        canVisit _ (_, Start) = Nothing
        canVisit _ (twice, Big x) = Just (twice, Big x)
        canVisit path (True, cave) | cave `notElem` path = Just (True, cave) 
                                   | otherwise = Nothing -- we've already visited a Small cave twice, and this one was in the path so we can't visit it again.
        canVisit path (False, cave)  | cave `notElem` path = Just (False, cave)
                                     | otherwise = Just (True, cave) -- it's not Big or Start, and it is already in the path...so we are up to second visit

