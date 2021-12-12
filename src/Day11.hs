module Day11
    ( part1
    , part2
    ) where

import Debug.Trace
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Char

type Point = (Int, Int)
type OctoMap = Map Point Int

sample = "11111\n19991\n19191\n19991\n11111"

part1 :: IO ()
part1 = do
    putStrLn "Day 11 part 1"
    ls <- lines <$> readFile "day11.txt"
    -- ls <- lines <$> readFile "day11_example.txt"
    let m = buildOctoMap ls
    let flashCount = applyStep m 100 0
    putStrLn $ show flashCount
    
part2 :: IO ()
part2 = do
    putStrLn "Day 11 part 2"
    ls <- lines <$> readFile "day11.txt"
    -- ls <- lines <$> readFile "day11_example.txt"
    let m = buildOctoMap ls
    let step = findSyncedFlashes m 0 0
    putStrLn $ show step

buildOctoMap :: [String] -> OctoMap
buildOctoMap ls = 
    Map.fromList pointTuples
    where
        rows = zip ls [0..]
        pointTuples = concatMap createPointTuples rows
        createPointTuples (row, y) = zip [(x, y) | x <- [0..]] (map digitToInt row)

applyStep :: OctoMap -> Int -> Int -> Int
applyStep _ 0 flashes = flashes
applyStep m count flashes = applyStep m' (count - 1) (flashes + stepSum)
                            where
                                (stepSum, m') = step m 

findSyncedFlashes :: OctoMap -> Int -> Int -> Int
findSyncedFlashes _ n 100 = n
findSyncedFlashes m n flashes = (traceShow (n, flashes) (findSyncedFlashes m' (n + 1) stepFlashes))
                                  where
                                    (stepFlashes, m') = step m

step :: OctoMap -> (Int, OctoMap)
step m =
    countFlashesAndReset flashedMap
    where
        points = [(x,y) | x <- [0..9], y <- [0..9]]
        flashedMap = foldl (\m pt -> updatePoint m pt (m Map.! pt)) m points

updatePoint :: OctoMap -> Point -> Int -> OctoMap
updatePoint m pt energy | energy + 1 == 10 = flashPoints m pt -- foldl (\m pt -> updatePoint m pt (m Map.! pt)) updatedMap adjacents
                        | otherwise = updatedMap    
                    where
                        onMap (x, y) = x >= 0 && x < 10 && y >= 0 && y < 10
                        adjacents (x, y) = [(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], onMap (x + dx, y + dy), (dx /= 0 || dy /= 0)]
                        updatedMap = Map.insert pt (energy + 1) m
                        flashPoints m pt = foldl (\m pt -> updatePoint m pt (m Map.! pt)) updatedMap (adjacents pt)

countFlashesAndReset :: OctoMap -> (Int, OctoMap)
countFlashesAndReset m  = Map.mapAccum needReset 0 m

needReset :: Int -> Int -> (Int, Int)
needReset flashes energy | energy > 9 = (flashes + 1, 0)
                         | otherwise = (flashes, energy)







-- incrementMap :: OctoMap -> (OctoMap, [Point])
-- incrementMap m  = 
--     foldl (\(m', flashes) pt -> incAndCollectFlashes flashes $ incrementAndFindFlash m' pt) (m, []) points
--     where
--         points = [(x,y) | x <- [0..9], y <- [0..9]]
--         incAndCollectFlashes flashes (m', Nothing) = (m', flashes)
--         incAndCollectFlashes flashes (m', Just flash) = (m', flash:flashes)

-- increment :: OctoMap -> Point -> (OctoMap, Int)
-- increment m pt =
--     (Map.insert pt newVal m, newVal)
--     where
--         newVal :: Int
--         newVal = (1 + (m Map.! pt))

-- incrementAndFindFlash :: OctoMap -> Point -> (OctoMap, Maybe Point)
-- incrementAndFindFlash m pt =
--     if val == 10 then
--         (m', Just pt)
--     else
--         (m', Nothing)
--     where
--         (m', val) = increment m pt

-- incrementAndFlash :: OctoMap -> Point -> OctoMap
-- incrementAndFlash m pt =
--     if val == 10 then
--         flash m' pt
--     else
--         m'
--     where
--         (m', val) = increment m pt

-- foldFlashes :: OctoMap -> [Point] -> OctoMap
-- foldFlashes m flashes =
--     foldl (\m pt -> flash m pt) m flashes

-- flash :: OctoMap -> Point -> OctoMap
-- flash m (x, y) =
--     foldl (\m pt' -> incrementAndFlash m pt') m adjacents
--     where
--         adjacents = [(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], (dx /= 0 || dy /= 0)]

-- mapAndFlashString :: (OctoMap, Int) -> Point -> String
-- mapAndFlashString (m, flashes) size = 
--     (toString m size) ++ "\n" ++ (show flashes) ++ "\n"

-- toString :: OctoMap -> Point -> String
-- toString m (w, h) =
--     unlines ls
--     where
--         ls = map (\y -> toLine y w m) [0..(h-1)]
--         toLine :: Int -> Int -> OctoMap -> String
--         toLine y w m = foldr (\x l -> (show $ Map.findWithDefault (-1) (x, y) m) ++ l) "" [0..(w-1)]


