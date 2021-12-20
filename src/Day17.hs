module Day17
  (part1,
  part2)
  where

import Data.Maybe
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
-- import Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as Map

type Point = (Int, Int)

-- target area: x=138..184, y=-125..-71
target = [(128, -125), (184, -71)]
bounds = ((138, 184), (-125, -71))

xBounds = fst bounds
minX = fst xBounds
maxX = snd xBounds
yBounds = snd bounds
minY = fst yBounds
maxY = snd yBounds

part1 :: IO ()
part1 = do
    putStrLn "Day 17 part 1"
    let minY = minimum $ map snd target
    let answer = maximum $ map snd $ takeWhile (\(_,y) -> y >= (minY - 200)) $ points (17, -(minY + 1))
    print answer

part2 :: IO ()
part2 = do
    putStrLn "Day 17 part 2"
    print $ length solutions

inTargetArea :: Point -> Bool
inTargetArea (x,y ) =
  x >= minX && x <= maxX &&
  y >= minY && y <= maxY

xVals :: Int -> Int -> [Int]
xVals x 0 = [x]
xVals x vx = x:xVals (x + vx) (vx - 1)

points (vx, vy) = points' (0, 0) (vx, vy)
  where
    points' (x, y) (vx, vy) = (x, y):points' (x + vx, y + vy) (subvx vx, vy - 1)
    subvx 0 = 0
    subvx vx = vx - 1


xAtT :: Int -> Int -> Int
xAtT vx t | t <= vx = vx * t - (t * (t - 1)) `div` 2
          | otherwise = vx * vx - (vx * (vx - 1)) `div` 2

yAtT :: Int -> Int -> Int
yAtT vy t = vy * t - (t * (t - 1)) `div` 2

ySolutions :: [(Int, Int)]
ySolutions = concat sols
  where
    vys = [minY .. 200]
    sols = map ySolution vys

ySolution :: Int -> [(Int, Int)]
ySolution vy = ySolution' vy 1 []

ySolution' :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
ySolution' vy t acc | yAtT vy t < (-125) = acc
                    | inYRange (yAtT vy t) = (t, vy):ySolution' vy (t + 1) acc
                    | otherwise = ySolution' vy (t + 1) acc

xSolutions :: Int -> [(Int, Int)]
xSolutions maxT = concat sols
  where
    vxs = [16 .. maxX]
    sols = map (xSolution maxT) vxs

xSolution :: Int -> Int -> [(Int, Int)]
xSolution maxT vx = xSolution' maxT vx 1 []

xSolution' :: Int -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
xSolution' maxT vx t acc | t > maxT = acc
                         | inXRange (xAtT vx t) = (t, vx):xSolution' maxT vx (t + 1) acc
                         | otherwise = xSolution' maxT vx (t + 1) acc

inRange :: Int -> Int -> Int -> Bool
inRange minV maxV v | v < minV = False
                    | v > maxV = False
                    | otherwise = True

inYRange = inRange minY maxY

inXRange = inRange minX maxX

solutions :: [(Int, Int)]
solutions =  Set.toList $ Set.fromList sols
-- solutions =  sols
  where
    sols = [(vx, vy) | (tx, vx) <- xSols, (ty, vy) <- ySols, tx == ty]
    xSols = xSolutions maxT
    ySols = ySolutions
    maxT = maximum $ map fst ySolutions

solve (vx, vy) = find (inTargetArea . fst) $ takeWhile (( >= minY) . snd . fst)  sols
  where
    ts = [1..]
    sols = map (\t -> ((xAtT vx t, yAtT vy t), t)) ts
