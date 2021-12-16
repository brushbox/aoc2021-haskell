module Day15
    ( part1
    , part2
    ) where

import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Char
import Data.List
-- import Data.List.Split
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Map.Strict as PQ
import qualified Data.Set as Set
-- import qualified Data.PQueue.Prio.Min as PQ
import Data.Graph.AStar

-- import Astar

type Point = (Int, Int)
type Path = [Point]
type RiskLevels = Map Point Int

part1 :: IO ()
part1 = do
    putStrLn "Day 15 part 1"
    input <- readFile "day15.txt"
    -- input <- readFile "day15_example.txt"
    putStrLn $ input
    let (riskLevels, destination) = buildRiskLevels input
    -- let result = astarSearch (0, 0) (destination ==) (nextNodes destination riskLevels) (manhattan destination)
    -- putStrLn $ show riskLevels


    -- print $ pt1 riskLevels
    -- putStrLn $ show result
    -- putStrLn $ show cost
    -- putStrLn $ show $ nextNodes destination riskLevels (0, 0)
    -- putStrLn $ show $ nextNodes destination riskLevels (1, 1)
    let path = aStar neighbours (distance riskLevels) (manhattan (maxX, maxY)) (destination ==) (0, 0)
    putStrLn $ show path
    let Just path' = path
    let answer = foldl (\s pt -> s + (riskLevels Map.! pt)) 0 path'
    putStrLn $ show answer

maxX = 99
maxY = 99

neighbours :: Point -> HashSet Point
neighbours (x,y) = HashSet.fromList [(x + dx, y + dy) | (dx, dy) <- [(0,1), (1, 0), (0,-1), (-1, 0)], (x + dx) >= 0 && (x + dx) <= maxX && (y + dy) >= 0 && (y + dy) <= maxY]

distance :: RiskLevels -> Point -> Point -> Int
distance riskLevels u v = riskLevels Map.! u

part2 :: IO ()
part2 = do
    putStrLn "Day 15 part 2"
    -- input <- readFile "day15.txt"
    -- input <- readFile "day15_example.txt"
    -- putStrLn $ input

buildRiskLevels :: String -> (RiskLevels, Point)
buildRiskLevels input = (riskLevels, destination)
  where
    riskLevels = Map.fromList allPairs
    allPairs = concatMap (\(row, y) -> buildRow row y) (zip rows [0..])
    buildRow row y = map (\(c, x) -> ((x, y), digitToInt c)) (zip row [0..])
    rows = lines input
    destination = fst $ last allPairs

-- pt1 = snd . maximum . path (Map.!?)

-- path getter grid = dijkstra
--   id
--   (\p -> [ (p', d) | p' <- neighbors p, Just d <- [getter grid p'] ])
--   (origin, 0)
--   (const False)

manhattan :: Point -> Point -> Int
manhattan (x1, y1) (x2, y2) = (abs (x1 - x2)) + (abs (y1 - y2))

-- nextNodes :: Point -> RiskLevels -> Point -> [(Point, Int)]
-- nextNodes (w, h) riskLevels (x, y) = zip points pointRisks
--   where
--     points = [(x + dx, y + dy) | (dx, dy) <- [(0, 1), (0, -1), (1, 0), (-1, 0)], onBoard (x + dx, y + dy)]
--     onBoard (x, y) = x >= 0 && y >= 0 && x < w && y < h
--     -- pointRisks = map (riskLevels Map.!) points
--     pointRisks = repeat (riskLevels Map.! (x,y))

-- https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
{-
In the following pseudocode algorithm, dist is an array that contains the current distances from the 
source to other vertices, i.e. dist[u] is the current distance from the source to the vertex u. 
The prev array contains pointers to previous-hop nodes on the shortest path from source to the given 
vertex (equivalently, it is the next-hop on the path from the given vertex to the source). 
The code u ← vertex in Q with min dist[u], searches for the vertex u in the vertex set Q that has the 
least dist[u] value. length(u, v) returns the length of the edge joining (i.e. the distance between) 
the two neighbor-nodes u and v. 
The variable alt on line 18 is the length of the path from the root node to the neighbor node v if it 
were to go through u. 
If this path is shorter than the current shortest path recorded for v, that current path is replaced 
with this alt path.

 function Dijkstra(Graph, source):
 2
 3      create vertex set Q
 4
 5      for each vertex v in Graph:            
 6          dist[v] ← INFINITY                 
 7          prev[v] ← UNDEFINED                
 8          add v to Q                     
 9      dist[source] ← 0                       
10     
11      while Q is not empty:
12          u ← vertex in Q with min dist[u]   
13                                             
14          remove u from Q
15         
16          for each neighbor v of u still in Q:
17              alt ← dist[u] + length(u, v)
18              if alt < dist[v]:              
19                  dist[v] ← alt
20                  prev[v] ← u
21
22      return dist[], prev[]

If we are only interested in a shortest path between vertices source and target, we can terminate the 
search after line 15 if u = target. 
Now we can read the shortest path from source to target by reverse iteration:

1  S ← empty sequence
2  u ← target
3  if prev[u] is defined or u = source:          // Do something only if the vertex is reachable
4      while u is defined:                       // Construct the shortest path with a stack S
5          insert u at the beginning of S        // Push the vertex onto the stack
6          u ← prev[u]                           // Traverse from target to source
-}

-- type PointSet = Set Point
-- type CostMap = Map Point Int
-- type PathMap = Map Point Point
-- dijkstra :: RiskLevels -> Point -> Point -> (CostMap, PathMap)
-- dijkstra riskLevels source target =
--   where
--     q = Set.singleton source
--     dist = Map.singleton source 0
--     prev = Map.empty
--     processQueue :: PointSet -> CostMap -> PathMap -> (CostMap, PathMap)
--     processQueue q dist prev
--       | Map.null q = (dist, prev)
--       | otherwise = 
--           let u = minQ q dist in ...
--     minQ q dist = 

-- -- snarfed this from https://topaz.github.io/paste/#XQAAAQAVCAAAAAAAAAA2m8ixrhLu7YJFrd2FLde+PAG1Aui2yN36LCvqT8ZOJmqbLYgEC+r5N94QUS0T9/8dZQVWN/Eq5jVDPAVZTN15zlecQ7+brnMx0LaVIQ1iAdiHKNUpCENXt4qagn0s3gBKcvdJBxmI0ljJMEPVVZgnTltQ0dz3zXfGEk2mTmTsI1Kzr37nQqoyrH1dej8lkPUzs38OpjkFZUs6SfvfHr5fXAShj7NHN8urglvwwXWVTbDpbFtskIB/ij7XYDKF8Q38AMACayG9RH25wTFDDZzwYRMMueAIbCyKAZBwKdKViJ+HpHJAPDUgKQQdJstw7kDjPixoDUGegAZ5swB3qd6mBmRIVEsFwhfYmW9d9hRRtDWYnQdZn+fomHedv1YcO+uDs92bLEtOlXNXg/7aCxb0MJHM/ap28Y+/YyBIf/Z9WNDLv5HG48hghEB8WGB2irKBafQojfsDh96VjpvDeblk2cDGl0QCNFn4FIVclBl4StsDNawqJ2NOGyOpd96QBadmRumlZgPbECyXFykzxcqVdF10ELFCpgKV1ixCcHlu1lDQg0+gMFdHIciApltgePau4eILwryWDvxxvTUW4Roai5y1uy1rRVYhFVcp8lp3k/GaQIxlfX5g0k512j75+zt1p5znuG0q8cYkxL52WVmN338LGUVLsQE5m0uKD5smQrxeyL2mo54zGrnly2mu+OMN+eNbElIAbUbW6Eaz3x88tBNjcdBvoUzKCATLFJgweGS9FxtFQsJWpXidhRYKQQypOBHQopBfk4WnkXfxXY9Y2Ha3kGDZWhYbS3kRN+1Mci1kePc0UlI0OJz6yWdX7CkO5UlJ7TQJ1tt2sBbCNEMrlwOCJBvkEpDBETjXfB7Gz6KbAiodL6kqsG9OHWSM9sPMJDEBXmZcVHlIcf/QF1IWWHe0KKDAEjS6YGVIPoWbqAHduf8lb3oSPwRuADj8fCId854RoKawsp7XxeI+/brFHM4jn+Jv6OzqH5GALyekCskIWmH1X0ePjB0nacDHRmoO8m0nE3ovAhg7MGJS+G08JGPtCjqxNieiOPCPq1K5njyJIrjHjbuSYN/MA0wUhcWD6Ct7XJYJ7LklBJe0YNNSUwnb+TI7iCowt8B0k4lhj9Csnn02qSk6ZMtIhXL6N/5TSj7Spr1UlnVkkv0jzQBMr01AG1FRO7pW4UW/f2musf411LK7mlUln4xZJKhTScdDH23qJy81a1yUFjH6NQUA29MOwPtyHDqu0PHdDLp6kCtax5lIJtnj4P4Lyb3HPYdx5loKhMrzV2XFRd0Hl8W8odBdCf/v2i07uhvctC+MZGNZvZbwrBQ69gImmGOzOlC+QsFYhom24iGN3hbkHkeVbxzfkaS2Rjfcb03+RS/iLxqVSuFdAdY2JR7PxDHh1Y+5lpt8gkL/5HF8FA==
-- -- Generalized version of Dijkstra's shortest paths algorithm.
-- -- but it didn't define what types it uses :-(
-- -- and I have no idea what :-> is
-- dijkstra
--   :: (Show v, Ord v, Eq v, Ord a, Eq a, Show a, Foldable f, Ord d, Num d)
--   => (a -> v) -- repr function for storing search elements in the visited set
--   -> (a -> f (a, d)) -- find neighbors and distance
--                           -- NOTE: the Int here is total g(v)+h(v),
--                           -- so set h(v)>0 if using A* with a consistent
--                           -- heuristic.
--   -> (a, d) -- start state and start distance
--   -> (v -> Bool) -- predicate for when we're done
--   -> [(a, d)]
-- dijkstra repr step (start, p) isDone = help Set.empty (PQ.singleton start p)
--  where
--   help seen queue = case PQ.findMin queue of
--     Nothing                -> []
--     Just (cur PQ.:-> dist) -> if isDone (repr cur)
--       then [(cur, dist)]
--       else if repr cur `Set.member` seen
--         then help seen (PQ.deleteMin queue)
--         else
--           let enq q (a, dist') = PQ.insertWith min a (dist + dist') q
--               queue' = foldl enq (PQ.deleteMin queue) (step cur)
--               seen'  = Set.insert (repr cur) seen
--           in  (cur, dist) : help seen' queue'