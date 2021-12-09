module Day9
    ( part1
    , part2
    ) where

import Debug.Trace
import qualified Data.Char as Char
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

data Seafloor = Seafloor Int Int [String]
type BasinPoints = Set Point
data Basin = Basin Point Int
    deriving Show

type Point = (Int, Int)
type Visited = [Point]

part1 :: IO ()
part1 = do
    putStrLn "Day 9 part 1"
    contents <- readFile "day9.txt"
    -- contents <- readFile "day9_example.txt"
    let seafloor = mapSeafloor contents
    let lps = lowPoints seafloor
    let rls = riskLevels lps seafloor
    putStrLn $ show $ sum rls

part2 :: IO ()
part2 = do
    putStrLn "Day 9 part 2"
    -- contents <- readFile "day9_example.txt"
    contents <- readFile "day9.txt"
    let seafloor = mapSeafloor contents
    let lps = lowPoints seafloor
    let basins = map (mapBasin seafloor) lps
    let sizes = reverse $ sort $ map (\(Basin _ size) ->  size) basins
    let answer = product $ take 3 sizes
    putStrLn $ show answer

mapBasin :: Seafloor -> Point -> Basin
mapBasin seafloor pt = Basin pt (Set.size (exploreBasin seafloor pt (Set.singleton pt)))

exploreBasin :: Seafloor -> Point -> BasinPoints -> BasinPoints
exploreBasin seafloor pt basinPoints =
    foldl (\bps pt' -> exploreBasin seafloor pt' (Set.insert pt' bps)) basinPoints points
    where
        points = filter (\pt -> notVisited pt && notPeak pt) $ quadrants pt
        notVisited pt = not (Set.member pt basinPoints)
        notPeak pt = (floorHeight seafloor pt) /= '9'

quadrants :: Point -> [Point]
quadrants (x, y) = [(x + dx, y + dy) | (dx, dy) <- [(0, -1), (0, 1), (1, 0), (-1, 0)]]

mapSeafloor :: String -> Seafloor
mapSeafloor contents =
    Seafloor width height rows
    where
        rows = lines contents
        width = length $ head rows
        height = length rows

lowPoints :: Seafloor -> [Point]
lowPoints seafloor@(Seafloor width height _ ) = [(x, y) | x <- [0..(width-1)], y <- [0..(height-1)], isLower seafloor (x, y)]

riskLevels :: [Point] -> Seafloor -> [Int]
riskLevels points seafloor = map (\pt -> (1 + (Char.digitToInt $ floorHeight seafloor pt))) points

isLower :: Seafloor -> Point -> Bool
isLower s point =
    all (pointHeight < ) surroundingHeights
    where
        surroundingHeights = map (floorHeight s) $ quadrants point
        pointHeight = floorHeight s point

floorHeight :: Seafloor -> Point -> Char
floorHeight s point =
    case (floorHeight' s point) of
        Nothing -> '9'
        Just c -> c

floorHeight' :: Seafloor -> Point -> Maybe Char
floorHeight' (Seafloor w h map) (x, y) 
    | x < 0 || x >= w = Nothing
    | y < 0 || y >= h = Nothing
    | otherwise = Just ((map !! y) !! x)
