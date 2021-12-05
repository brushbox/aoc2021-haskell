module Day5
    ( part1
    , part2
    ) where

import Data.List
import Data.List.Split
import qualified Data.Map.Strict as Map

type Point = (Int, Int)
type Line = (Point, Point)

part1 :: IO ()
part1 = do
    putStrLn "Day 5 part 1"
    textLines <- lines <$> readFile "day5.txt"
    let ls = map parseLine textLines
    let hv = filter isHorV ls
    let m = foldl (\m line -> drawLine m line) Map.empty hv
    let result = Map.foldl countOverlaps 0 m
    putStrLn $ show result

countOverlaps :: Int -> Int -> Int
countOverlaps sum overlaps =
    if overlaps >= 2 then
        sum + 1
    else
        sum

parseLine :: String -> Line
parseLine = toLine . readLine

readLine :: String -> [[Int]]
readLine str = map readPoint $ splitOn " -> " str

readPoint :: String -> [Int]
readPoint str = map read $ splitOn "," str

toLine :: [[Int]] -> Line
toLine (s:f:_) = (toPoint s, toPoint f)
toLine _ = error "Ill-formed line data"

toPoint :: [Int] -> Point
toPoint (x:y:_) = (x, y)
toPoint _ = error "Ill-formed point data"

isHorV :: Line -> Bool
isHorV ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

drawLine :: Map.Map Point Int -> Line -> Map.Map Point Int
drawLine mp line = foldl plotPoint mp (lineToPoints line)

plotPoint :: Map.Map Point Int -> Point -> Map.Map Point Int
plotPoint mp pt = Map.insertWith (+) pt 1 mp

lineToPoints :: Line -> [Point]
lineToPoints (startPt, endPt) = lineToPoints' startPt endPt

lineToPoints' :: Point -> Point -> [Point]
lineToPoints' startPt endPt =
    if startPt /= endPt then
        (startPt : lineToPoints' (closer startPt endPt) endPt)
    else
        [startPt]

closer :: Point -> Point -> Point
closer (x1, y1) (x2, y2) =
    (x1 + (dir x1 x2), y1 + (dir y1 y2))

dir :: Int -> Int -> Int
dir a b =
    if a < b then
        1
    else if a > b then
        -1
    else
        0

part2 :: IO ()
part2 = do
    putStrLn "Day 5 part 2"
    textLines <- lines <$> readFile "day5.txt"
    let ls = map parseLine textLines
    let m = foldl (\m line -> drawLine m line) Map.empty ls
    let result = Map.foldl countOverlaps 0 m
    putStrLn $ show result