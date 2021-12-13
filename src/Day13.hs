module Day13
    ( part1
    , part2
    ) where

import Data.List.Split
import Data.Set (Set)
import qualified Data.Set as Set

type Point = (Int, Int)
type PointSet = Set Point
data Fold = Vertical Int | Horizontal Int
    deriving Show

part1 :: IO ()
part1 = do
    putStrLn "Day 13 part 1"
    input <- readFile "day13.txt"
    -- input <- readFile "day13_example.txt"
    let (points, folds) = parseInput input
    let foldedSet = foldSet (Set.fromList points) (head folds)
    putStrLn $ show (Set.size foldedSet)

part2 :: IO ()
part2 = do
    putStrLn "Day 13 part 2"
    input <- readFile "day13.txt"
    let (points, folds) = parseInput input
    let foldedSet = foldl foldSet (Set.fromList points) folds
    -- putStrLn $ show foldedSet
    putStrLn $ showPointSet foldedSet

parseInput :: String -> ([Point], [Fold])
parseInput input = (coords, folds)
    where
        coords = parsePoints $ takeWhile notEmpty (lines input)
        folds = parseFolds $ tail $ dropWhile notEmpty  (lines input)
        notEmpty = (/= 0) . length

parsePoints :: [String] -> [Point]
parsePoints = map parsePoint

parsePoint :: String -> Point
parsePoint s = (x, y)
    where
        [x, y] = map read $ splitOn "," s

parseFolds :: [String] -> [Fold]
parseFolds = map parseFold

parseFold :: String -> Fold
parseFold s = constructor value
    where
        constructor = orientation (last left)
        value = read right
        orientation 'x' = Vertical 
        orientation 'y' = Horizontal
        [left,right] = splitOn "=" s
        
foldSet :: PointSet -> Fold -> PointSet
foldSet points (Vertical x) = Set.map foldVertical points
    where
        foldVertical (px, py) | px < x = (px, py)
                              | otherwise = (x - (px - x), py)
foldSet points (Horizontal y) = Set.map foldHorizontal points
    where
        foldHorizontal (px, py) | py < y = (px, py)
                                | otherwise = (px, y - (py - y))

showPointSet :: PointSet -> String
showPointSet points =
    unlines rows
    where
        rows = map row [0..maxY]
        row y = map (displayPoint y) [0..maxX]
        displayPoint y x | (x, y) `Set.member` points = '#'
                         | otherwise = '.'
        maxX = head xs
        maxY = head ys
        xs = Set.toDescList $ Set.map (\(x, _) -> x) points
        ys = Set.toDescList $ Set.map (\(y, _) -> y) points
