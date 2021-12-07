module Day7
    ( part1
    , part2
    ) where

import Data.List
import Data.List.Split

type Crabs = [Int]
type PosFN = Crabs -> Int
type CostFN = Int -> Int -> Int

part1 :: IO ()
part1 = do
    putStrLn "Day 7 part 1"
    showFuelCost median distance

part2 :: IO ()
part2 = do
    putStrLn "Day 7 part 2"
    showFuelCost average increasingCost

showFuelCost :: PosFN -> CostFN -> IO ()
showFuelCost posFn costFn = do
    contents <- readFile "day7.txt"
    let vals = map read $ splitOn "," contents
    let result = fuelCost posFn costFn vals
    putStrLn $ show result

median :: PosFN
median vals
    | (odd count) = sorted !! middle
    | otherwise = avgMiddle
    where
        count = length vals
        sorted = sort vals
        middle = count `div` 2 - 1
        avgMiddle = ((sorted !! middle) + (sorted !! (middle + 1))) `div` 2

average :: PosFN
average crabs = (sum crabs) `div` (length crabs)

fuelCost :: PosFN -> CostFN -> Crabs -> Int
fuelCost posFn costFn crabs = sum $ map (costFn (posFn crabs)) crabs

distance :: CostFN
distance a b = abs $ (a - b)

increasingCost :: CostFN
increasingCost pos a =
    n * (n + 1) `div` 2
    where
        n = abs $ (pos - a)