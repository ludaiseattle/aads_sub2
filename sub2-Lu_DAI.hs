{-
  G54PAD Project in Advanced Algorithms and Data Structures
    Autumn 2022

  Assignment 2 
    Graph Algorithms

  Student Name: Lu DAI
  Student ID: 20350376

  Complete this Haskell file by providing definitions
  of the following functions:

  adjList
  adjMatrix

  adjListW
  adjMatrixW

  dijkstra
  floydWarshall

  You are allowed to define any other auxiliary function you need.

-}


module Graphs where

import Numeric

-- We assume that the vertices are numbered 0 ... n-1

{- In and adjacency list al, the i-th element al!!i
   is a list of all the vertices j such that
   there is an edge from i to j
-}

type AdjList = [[Int]]

{- In and adjacencly matrix am, the element with
   coordinates i,j, that is am!!i!!j
   is True if there is an edge from i to j
      False if there is no edge from i to j
-}

type AdjMatrix = [[Bool]]

-- Suppose we're given a graph as a list of edges (i,j)
-- Generate the Adjacency List and Adjacencty Matrix representations

-- GENERATION OF ADJACENCY LIST

fillGap :: Int -> AdjList -> AdjList
fillGap n l =
   if length l > n then l 
   else l ++ take (n - length l +1) (repeat [])


insertTuple :: (Int,Int)->AdjList -> AdjList
insertTuple tup l = insertEle tup (fillGap (fst tup) l)
       where insertEle tup list = (take (fst tup) list) ++ [snd tup:(list!!fst tup)] ++ (drop (fst tup+1) list)

adjList :: [(Int,Int)] -> AdjList
adjList [] = []
adjList (x:xs) = insertTuple (fst x, snd x)  (adjList xs)

-- GENERATION OF ADJACENCY MATRIX
fillByBool :: Int -> [Bool] -> [Bool]
fillByBool n l = fill n (fillBoolGap n l)
   where fillBoolGap n l = if length l > n then l else l ++ take (n - length l +1) (repeat False)
         fill n list= take n list ++ [True] ++ drop (n+1) list

fillBoolGap :: Int -> AdjMatrix -> AdjMatrix
fillBoolGap n l =
   if length l > n then l 
   else l ++ take (n - length l +1) (repeat [])

insertMatrix :: (Int,Int) -> AdjMatrix -> AdjMatrix
insertMatrix tup l = insertValue tup (fillBoolGap (fst tup) l)
   where insertValue tup list = (take (fst tup) list) ++ [fillByBool (snd tup) (list!!fst tup)] ++ (drop (fst tup+1) list)

adjMatrix :: [(Int,Int)] -> AdjMatrix
adjMatrix [] = []
adjMatrix (x:xs) = insertMatrix (fst x, snd x) (adjMatrix xs)

--------------------------------------------------------

-- WEIGHTED GRAPHS: every edge has a "weight"

{- In an adjacency list al, the i-th element al!!i
   contains all the pairs (j,w) such that
   there is an edge from i to j with weight w
-}

type WAdjList = [[(Int,Float)]]

{- In an adjacency matrix am, the element with
   coordinates i,j is
     Nothing if there is no edge from i to j
     (Just w) if there is an edge from i to j with weight w
-}

type WAdjMatrix = [[Maybe Float]]

{- We can also represent a weighted graphs by a list of edges
   (i,j,w) denotes an edge from i to j with weight w
-}

type Edges = [(Int,Int,Float)]


first::(a,b,c) -> a
first (x, _, _) = x

second :: (a,b,c) -> b
second (_,y,_) = y

third :: (a,b,c) -> c
third (_,_,z) = z

-- GENERATION OF ADJACENCY LIST
--   from a list of edges

fillLWGap :: Int -> WAdjList -> WAdjList
fillLWGap n l =
   if length l > n then l 
   else l ++ take (n - length l +1) (repeat [])

insertListW :: (Int, Int, Float) -> WAdjList -> WAdjList
insertListW tup l = insertLWEle tup (fillLWGap (first tup) l)
   where insertLWEle tup list = (take (first tup) list) ++ [(second tup, third tup):(list!!first tup)] ++ (drop (first tup+1) list)

adjListW :: Edges -> WAdjList
adjListW [] = []
adjListW (x:xs) = insertListW (first x, second x, third x)  (adjListW xs)

-- test: adjListW [(1,2,0.1),(1,1,0.2),(2,3,0.5)]

-- GENERATION OF ADJACENCY MATRIX
--   from a list of edges

fillByBoolW :: (Int, Float) -> [Maybe Float] -> [Maybe Float]
fillByBoolW (n, w) l = fill (n, w) (fillBoolGap n l)
   where fillBoolGap n l = if length l > n then l else l ++ take (n - length l +1) (repeat Nothing)
         fill (n, w) list= take n list ++ [Just w] ++ drop (n+1) list

fillBoolGapW :: Int -> WAdjMatrix -> WAdjMatrix
fillBoolGapW n l =
   if length l > n then l 
   else l ++ take (n - length l +1) (repeat [])

insertMatrixW :: (Int,Int, Float) -> WAdjMatrix -> WAdjMatrix
insertMatrixW tup l = insertValueW tup (fillBoolGapW (first tup) l)
   where insertValueW tup list = (take (first tup) list) ++ [fillByBoolW (second tup, third tup) (list!!first tup)] ++ (drop (first tup+1) list)

adjMatrixW :: Edges -> WAdjMatrix
adjMatrixW [] = []
adjMatrixW (x:xs) = insertMatrixW (first x, second x, third x) (adjMatrixW xs)

-- test: adjMatrixW [(1,1,0.1), (1,0,0.2),(2,1,0.3),(3,3,0.4)]

-- DIJKSTRA'S ALGORITHM

{- 
   Given an adjacencly list al and a source vertex s
   return the list of minimum distances of vertices from s:
   (dijkstra al s)!!j is the minimum distance from s to j
-}

type DefinitedDisVec = [Maybe Float]
type UndefDisVec = [(Int, Maybe Float)] --Int: vertice

initDefV :: Int -> DefinitedDisVec
initDefV s = [if x < s then Nothing else Just 0.0| x <- [0..s]]

initUndefV :: Int -> UndefDisVec
initUndefV len = [(x, Nothing)|x <- [0..len-1]]

findShortestV :: UndefDisVec -> (Int, Maybe Float)
findShortestV [x] = x
findShortestV (x:xs)
   | snd x == Nothing = minV
   | snd minV == Nothing = x
   | snd x < snd minV = x
   | otherwise = minV
   where minV = findShortestV xs   

updateDefV :: DefinitedDisVec -> (Int, Maybe Float) -> DefinitedDisVec
updateDefV defVOld tup = (take (fst tup) defVOld) ++ [snd tup] ++ (drop (fst tup+1) defVOld)

findInOld :: [(Int, Float)] -> Int -> Maybe (Int, Maybe Float)
findInOld [] _ = Nothing
findInOld (x:xs) v = if fst x == v then Just (fst x, Just (snd x)) else findInOld xs v

compareW :: Maybe (Int, Maybe Float) -> (Int, Maybe Float) -> Float -> (Int, Maybe Float)
compareW new old oldW = if oldW + snd new >= snd old then old else (fst new, Just (oldW + snd new))

updateUndefV :: Float -> UndefDisVec -> [(Int, Float)] -> UndefDisVec
updateUndefV oldWeight oriL weightL = 
   [ let result = findInOld weightL (fst x) 
          in if result == Nothing then x else (compareW result x oldWeight) | x <- oriL ]

deleteOldV :: UndefDisVec -> Int -> UndefDisVec
deleteOldV l v = [ (fst x) /= v  | x <- l]

dijLoop :: DefinitedDisVec -> UndefDisVec -> WAdjList -> DefinitedDisVec
dijLoop defV [] _= defV
dijLoop defV undefV weightL= 
   let shortV = findShortestV undefV 
   in dijLoop (updateDefV defV shortV) (deleteOldV (updateUndefV (snd shortV) undefV weightL!!(fst shortV)) (fst shortV)) weightL

dijkstra :: WAdjList -> Int -> [Maybe Float]
dijkstra [] s = [] 
dijkstra l s =
   let defV = initDefV s   
       undefV = updateUndefV 0.0 (initUndefV (length l)) l!!s
   in dijLoop defV undefV l

-- FLOYD-WARSHALL ALGORITHM

{-
   Given an adjacency matrix am, return the matrix of minimum distances:
   (floydWarshall am)!!i!!j is
     Nothing if there is no path from i to j
     (Just x) if the shortest path from i to j has length x
-}

--floydWarshall :: WAdjMatrix -> WAdjMatrix
