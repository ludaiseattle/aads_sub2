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
       where insertEle tup list = (take (fst tup) list) ++ 
               [snd tup:(list!!fst tup)] ++ (drop (fst tup+1) list)

adjList :: [(Int,Int)] -> AdjList
adjList [] = []
adjList (x:xs) = insertTuple (fst x, snd x)  (adjList xs)

-- GENERATION OF ADJACENCY MATRIX
fillByBool :: Int -> [Bool] -> [Bool]
fillByBool n l = fill n (fillBoolGap n l)
   where fillBoolGap n l = if length l > n then l else l ++ 
            take (n - length l +1) (repeat False)
         fill n list= take n list ++ [True] ++ drop (n+1) list

fillBoolGap :: Int -> AdjMatrix -> AdjMatrix
fillBoolGap n l =
   if length l > n then l 
   else l ++ take (n - length l +1) (repeat [])

insertMatrix :: (Int,Int) -> AdjMatrix -> AdjMatrix
insertMatrix tup l = insertValue tup (fillBoolGap (fst tup) l)
   where insertValue tup list = (take (fst tup) list) ++ 
            [fillByBool (snd tup) (list!!fst tup)] ++ (drop (fst tup+1) list)

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
   where insertLWEle tup list = (take (first tup) list) ++ 
            [(second tup, third tup):(list!!first tup)] ++ 
            (drop (first tup+1) list)

adjListW :: Edges -> WAdjList
adjListW [] = []
adjListW (x:xs) = insertListW (first x, second x, third x)  (adjListW xs)

-- test: adjListW [(1,2,0.1),(1,1,0.2),(2,3,0.5)]

-- GENERATION OF ADJACENCY MATRIX
--   from a list of edges

fillByBoolW :: (Int, Float) -> [Maybe Float] -> [Maybe Float]
fillByBoolW (n, w) l = fill (n, w) (fillBoolGap n l)
   where fillBoolGap n l = if length l > n then l else l ++ 
            take (n - length l +1) (repeat Nothing)
         fill (n, w) list= take n list ++ [Just w] ++ drop (n+1) list

fillBoolGapW :: Int -> WAdjMatrix -> WAdjMatrix
fillBoolGapW n l =
   if length l > n then l 
   else l ++ take (n - length l +1) (repeat [])

insertMatrixW :: (Int,Int, Float) -> WAdjMatrix -> WAdjMatrix
insertMatrixW tup l = insertValueW tup (fillBoolGapW (first tup) l)
   where insertValueW tup list = (take (first tup) list) ++ 
            [fillByBoolW (second tup, third tup) (list!!first tup)] ++ 
            (drop (first tup+1) list)

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
type UndefDisVec = [(Int, Float)] --Int: vertice -1: Nothing

initDefV :: Int -> Int -> DefinitedDisVec
initDefV s len = [if x == s then Nothing else Just 0.0| x <- [0..len]]

initUndefV :: Int -> UndefDisVec
initUndefV len = [(x, -1)|x <- [0..len-1]]

findShortestV :: UndefDisVec -> (Int, Float)
findShortestV [x] = x
findShortestV (x:xs)
   | snd x == -1 = minV
   | snd minV == -1 = x
   | snd x < snd minV = x
   | otherwise = minV
   where minV = findShortestV xs   

updateDefV :: DefinitedDisVec -> (Int, Float) -> DefinitedDisVec
updateDefV defVOld tup = (take (fst tup) defVOld) ++ [if snd tup == 
   -1 then Nothing else Just (snd tup)] ++ (drop (fst tup+1) defVOld)

iSUpdate :: [(Int, Float)] -> Int -> (Int, Float)
iSUpdate [] _ = (-1, 99999)
iSUpdate (x:xs) v = if fst x == v then x else iSUpdate xs v

compareW :: (Int, Float) -> (Int, Float) -> Float -> (Int, Float)
compareW new old vOldW 
   | snd old == -1 = (fst new, vOldW + snd new)
   | vOldW + snd new >= snd old = old 
   | otherwise = (fst new, vOldW + snd new)

updateUndefV :: Float -> UndefDisVec -> [(Int, Float)] -> UndefDisVec
updateUndefV vOldWeight currUndef currVWeightLst = 
   [ let result = iSUpdate currVWeightLst (fst x) 
          in if fst result == -1 then x else 
            (compareW result x vOldWeight) | x <- currUndef ]

deleteOldV :: UndefDisVec -> Int -> UndefDisVec
deleteOldV l v = [ x | x <- l, fst x /= v]

dijLoop :: DefinitedDisVec -> UndefDisVec -> WAdjList -> DefinitedDisVec
dijLoop defV [] _= defV
dijLoop defV undefV weightL= 
   let shortV = findShortestV undefV 
   in dijLoop (updateDefV defV shortV) (deleteOldV 
   (updateUndefV (snd shortV) undefV (weightL!!(fst shortV))) (fst shortV)) weightL

dijkstra :: WAdjList -> Int -> [Maybe Float]
dijkstra [] s = [] 
dijkstra l s =
   let defV = initDefV s (length l - 1)  
       undefV = updateUndefV 0.0 (initUndefV (length l)) (l!!s)
   in if (s + 1) > length l then [] else dijLoop defV undefV l
-- test: updateUndefV 0.0 [(0,-1.0),(1,-1.0),(2,-1.0),(3,-1.0),(4,-1.0)] [(1,1),(2,1)]
-- test: 


-- FLOYD-WARSHALL ALGORITHM

{-
   Given an adjacency matrix am, return the matrix of minimum distances:
   (floydWarshall am)!!i!!j is
     Nothing if there is no path from i to j
     (Just x) if the shortest path from i to j has length x
-}



minDis :: Int -> Int -> Int -> Float
minDis k i j w = min' (w!!i!!j) (w!!i!!k and' w!!k!!j)

minIJ :: [Float] -> Float
minIJ [x] = x
minIJ (x:xs) = let res = minIJ xs where if x <= res then x else res

floydLoopK :: Int -> Int -> WAdjMatrix -> Maybe Float
floydLoopK i k w = minIJ [minDis k i j w| k <- [0..(length w -1)]] -- just

floydLoopJ :: Int -> WAdjMatrix -> [Maybe Float]
floydLoopJ i w = [floydLoopK i j w | j <- [0..(length w -1)]]

floydLoopI :: WAdjMatrix -> WAdjMatrix
floydLoopI w = [floydLoopJ i w | i <- [0..(length w -1)]]

floydWarshall :: WAdjMatrix -> WAdjMatrix
floydWarshall w =  if length w == 0 then [] else floydLoopK w