module Astar (astarSearch) where

import Debug.Trace
import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
-- import Data.Hashable (Hashable)
import Data.List (foldl')
import Data.Maybe (fromJust)

astarSearch :: (Eq a, Ord a) => a -> (a -> Bool) -> (a -> [(a, Int)]) -> (a -> Int) -> Maybe (Int, [a])
astarSearch startNode isGoalNode nextNodeFn heuristic =
  astar (PQ.singleton (traceShow (heuristic startNode) (heuristic startNode)) (startNode, 0))
         Set.empty (Map.singleton startNode 0) Map.empty
  where
    astar pq seen gscore tracks
      | PQ.null pq           = Nothing
      | isGoalNode node      = Just (gcost, findPath tracks node)
      | Set.member node seen = astar pq' seen gscore tracks
      | otherwise            = astar pq'' seen' gscore' tracks'
      where
        (node, gcost) = snd . PQ.findMin $ pq
        pq'           = PQ.deleteMin pq
        seen'         = Set.insert node seen
        successors    = 
          filter (\(s, g, _) -> not (Set.member s seen') &&
                    (not (s `Map.member` gscore) || g < (fromJust . Map.lookup s $ gscore)))
          $ successorsAndCosts node gcost
        pq''    = foldl' (\q (s, g, h) -> PQ.insert (g + h) (s, g) q) pq' successors
        gscore' = foldl' (\m (s, g, _) -> Map.insert s g m) gscore successors
        tracks' = foldl' (\m (s, _, _) -> Map.insert s node m) tracks successors
    successorsAndCosts node gcost = map (\(s, g) -> (s, gcost + g, heuristic s)) . nextNodeFn $ node
    findPath tracks node          = if Map.member node tracks
      then findPath tracks (fromJust . Map.lookup node $ tracks) ++ [node]
      else [node]