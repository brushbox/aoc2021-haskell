module Day15
    ( part1
    , part2
    ) where

import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Char
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Graph.AStar

type Point = (Int, Int)
type Path = [Point]
type RiskLevels = Map Point Int

part1 :: IO ()
part1 = do
    putStrLn "Day 15 part 1"
    input <- readFile "day15.txt"
    -- input <- readFile "day15_example.txt"
    let (riskLevels, destination) = buildRiskLevels input
    let path = aStar (neighbours destination) (distance riskLevels) (manhattan destination) (destination ==) (0, 0)
    let Just path' = path
    let answer = foldl (\s pt -> s + (riskLevels Map.! pt)) 0 path'
    putStrLn $ show answer

neighbours :: Point -> Point -> HashSet Point
neighbours (maxX, maxY) (x,y) = HashSet.fromList [(x + dx, y + dy) | (dx, dy) <- [(0,1), (1, 0), (0,-1), (-1, 0)], (x + dx) >= 0 && (x + dx) <= maxX && (y + dy) >= 0 && (y + dy) <= maxY]

distance :: RiskLevels -> Point -> Point -> Int
distance riskLevels u v = riskLevels Map.! u

part2 :: IO ()
part2 = do
    putStrLn "Day 15 part 2"
    input <- readFile "day15.txt"
    -- input <- readFile "day15_example.txt"
    let (riskLevels, destination) = buildRiskLevels5x5 input
    let path = aStar (neighbours destination) (distance riskLevels) (manhattan destination) (destination ==) (0, 0)
    let Just path' = path
    let answer = foldl (\s pt -> s + (riskLevels Map.! pt)) 0 path'
    putStrLn $ show answer

buildRiskLevels :: String -> (RiskLevels, Point)
buildRiskLevels input = (riskLevels, destination)
  where
    riskLevels = Map.fromList (buildRiskLevel 0 0 input)
    (destination, _) = Map.findMax riskLevels

manhattan :: Point -> Point -> Int
manhattan (x1, y1) (x2, y2) = (abs (x1 - x2)) + (abs (y1 - y2))

buildRiskLevels5x5 :: String -> (RiskLevels, Point)
buildRiskLevels5x5 input = (riskLevels, destination)
  where
    riskLevels = Map.fromList allPairs
    allPairs = concatMap (\(xit, yit) -> buildRiskLevel xit yit input) tiles
    tiles = [(x, y) | x <- [0..4], y <- [0..4]]
    (destination, _) = Map.findMax riskLevels

buildRiskLevel :: Int -> Int -> String -> [(Point, Int)]
buildRiskLevel xit yit input = concatMap buildRow (zip rows [0..])
  where
    buildRow (row, y) = map (\(c, x) -> (point x y, calcValue c)) (zip row [0..])
    point x y = (x + (width * xit), y + (height * yit))
    calcValue c = ((digitToInt c) + xit + yit - 1) `mod` 9 + 1
    rows = lines input
    width = length $ head rows
    height = length rows

showRiskLevels :: RiskLevels -> String
showRiskLevels riskLevels = unlines rows
  where
    rows = map (\y -> row y) [0..height]
    row y = foldr (\x s -> (intToDigit (riskLevels Map.! (x, y))):s) [] [0..width]
    ((width, height), _) = Map.findMax riskLevels
