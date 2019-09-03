module EdgeGeographyGame where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Set (Set)
import qualified Data.Set as Set

-- You may import useful modules here.

{- The input is a list of adjacency lists, e.g.,
   [ (0, [1, 2]) , (1, [0]) , (2, [1]), (3, []) ]
   means 0->1, 0->2, 1->0, 2->1, 3 has no outgoing edges.

   goodFirstVertices takes this input and computes the choices for the first
   vertex so the first player is destined to win.
-}
goodFirstVertices :: [(Int, [Int])] -> [Int]
goodFirstVertices inp
    | inp == [] = []
    | otherwise = testAllStartPoints convertedGraph startPoints
    where convertedGraph = convertGraph inp
          startPoints = Map.keys convertedGraph

{- Convert the graph from a list of adjacency lists to
   a map of vertex -> set of neighbours
-}
convertGraph :: [(Int, [Int])] -> Map Int (Set Int)
convertGraph lst = Map.map (Set.fromList) (Map.fromList lst)

{- Delete an edge in the graph

   toIdx -- the index of the to-node in the set pointed to by the
   from-node
-}
deleteEdge :: (Map Int (Set Int)) -> Int -> Int -> (Map Int (Set Int))
deleteEdge graph fromNode toNode = Map.update del fromNode graph
    where del s = Just (Set.delete toNode s)

{- Test if the player who made the last move is destined to win if going
   to any of the next states (vertice) with index >= nextIdx. The nextIdx
   parameter is used for recursion.

   nextIdx -- the index of the next vertex to test in the set of the neighbours of
   "curr"
   nextVertices -- the neighbours of "curr"
-}
testState :: (Map Int (Set Int)) -> Int -> Int -> (Set Int) -> Bool
testState graph curr nextIdx nextVertices
    | nextIdx >= Set.size nextVertices = True
    | not (testAllNextStates reducedGraph nextVal_0 0 nextNextVertices_0) = False
    | otherwise = testState graph curr (nextIdx+1) nextVertices
    where nextVal_0 = Set.elemAt nextIdx nextVertices
          reducedGraph = (deleteEdge graph curr nextVal_0)
          Just nextNextVertices_0 = Map.lookup nextVal_0 reducedGraph

{- Test if the play who makes the next move has a winning strategy (i.e. There
   exists at least one choice of next move through which he will win at the end.)

   idx -- the index of the next vertex to test in the set of the neighbours of
   "fromNode"
   nextVertices -- the neighbours of "fromNode"
-}
testAllNextStates :: (Map Int (Set Int)) -> Int -> Int -> (Set Int) -> Bool
testAllNextStates graph fromNode idx nextVertices
    | idx >= Set.size nextVertices = False
    | testState reducedGraph nextVal_0 0 nextNextVertices_0 = True
    | otherwise = testAllNextStates graph fromNode (idx+1) nextVertices
    where nextVal_0 = Set.elemAt idx nextVertices
          reducedGraph = (deleteEdge graph fromNode nextVal_0)
          Just nextNextVertices_0 = Map.lookup nextVal_0 reducedGraph

testAllStartPoints :: (Map Int (Set Int)) -> [Int] -> [Int]
testAllStartPoints graph (firstSP:remainingSP)
    | testState graph firstSP 0 nextVertices = (firstSP : remaining)
    | otherwise = remaining
    where remaining
            | remainingSP == [] = []
            | otherwise = testAllStartPoints graph remainingSP
          Just nextVertices = Map.lookup firstSP graph