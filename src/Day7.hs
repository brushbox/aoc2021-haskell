module Day7
    ( part1
    , part2
    ) where

import Data.List
import Data.List.Split

part1 :: IO ()
part1 = do
    putStrLn "Day 7 part 1"
    showFuelCost median distance

part2 :: IO ()
part2 = do
    putStrLn "Day 7 part 2"
    showFuelCost average increasingCost

showFuelCost :: ([Int] -> Int) -> (Int -> Int -> Int) -> IO ()
showFuelCost posFn costFn = do
    contents <- readFile "day7.txt"
    let vals = map read $ splitOn "," contents
    let result = fuelCost posFn costFn vals
    putStrLn $ show result

median :: [Int] -> Int
median vals
    | (odd count) = sorted !! middle
    | otherwise = avgMiddle
    where
        count = length vals
        sorted = sort vals
        middle = count `div` 2 - 1
        avgMiddle = ((sorted !! middle) + (sorted !! (middle + 1))) `div` 2

average :: [Int] -> Int
average crabs = (sum crabs) `div` (length crabs)

fuelCost :: ([Int] -> Int) -> (Int -> Int -> Int) -> [Int] -> Int
fuelCost posFn costFn crabs = sum $ map (costFn (posFn crabs)) crabs

distance :: Int -> Int -> Int
distance a b = abs $ (a - b)

increasingCost :: Int -> Int -> Int
increasingCost pos a =
    n * (n + 1) `div` 2
    where
        n = abs $ (pos - a)