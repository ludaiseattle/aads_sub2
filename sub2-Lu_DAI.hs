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
--Test: l = [(0,0), (0,1), (0, 2), (0,5), (1,1), (1,3), (2,2), (2,3), (2,4), (2,5), (3,3), (3,4), (4,4), (5,0), (5,4), (5,5)]
-- Test: adjList l 

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
--Test: l = [(0,0), (0,1), (0, 2), (0,5), (1,1), (1,3), (2,2), (2,3), (2,4), (2,5), (3,3), (3,4), (4,4), (5,0), (5,4), (5,5)] 
--Test: adjMatrix l

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
-- test: updateUndefV 0.0 [(0,-1.0),(1,-1.0),(2,-1.0),(3,-1.0),(4,-1.0)] [(1,1),(2,1)]

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
-- test: wadjl = [[(0, 0.0), (1, 1.0), (2, 2.0), (5, 10.0)], [(1, 0.0), (3, 5.0)], [(2, 0.0), (3, 1.0), (4, 1.0), (5, 4.0)], [(3, 0.0), (4, 4.0)], [(4, 0.0)], [(0, 1.0), (4, 4.0), (5, 0.0)]]
-- test: dijkstra wadjl 0
-- test: dijkstra wadjl 5

-- FLOYD-WARSHALL ALGORITHM

{-
   Given an adjacency matrix am, return the matrix of minimum distances:
   (floydWarshall am)!!i!!j is
     Nothing if there is no path from i to j
     (Just x) if the shortest path from i to j has length x
-}

plus :: Maybe Float -> Maybe Float -> Maybe Float
plus a b = pure (+) <*> a <*> b
-- Test: plus Nothing (Just 4)
-- Test: plus (Just 5) (Just 6)

min' :: (Monad m, Ord a) => m a -> m a -> m a
min' x y = do
   m <- x
   n <- y
   if m < n then return m else return n
-- Test: min' Nothing (Just 5)
-- Test: min' (Just 4) (Just 0)

minDis :: Int -> Int -> Int -> WAdjMatrix -> Maybe Float
minDis i j k w = 
   let a = (w!!i!!j) 
       b = (plus (w!!i!!k) (w!!k!!j)) 
   in if a == Nothing && b /= Nothing then b 
   else if b == Nothing && a /= Nothing then a 
   else min' a b 
-- Test: minDis 0 2 1 [[Just 0.0,Just 2.2,Just 10.0],[Just 2.0,Nothing,Just 2.0],[Just 6.5,Just 2.0]]

minIJ :: [Maybe Float] -> Maybe Float
minIJ [x] = x
minIJ (x:xs) = 
   let res = minIJ xs 
   in if x == Nothing && res /= Nothing then res 
   else if res == Nothing && x /= Nothing then x 
   else min' x res 
-- Test: minIJ [Just 3.3, Nothing, Just 0, Just 2.2]

updateJ :: [Maybe Float] -> Int -> Maybe Float -> [Maybe Float]
updateJ l j val = take j l ++ [val] ++ drop (j+1) l
-- Test: updateJ [Just 1, Just 2, Just 4] 2 (Just 3)

updateMatrix :: WAdjMatrix -> Int -> Int -> Maybe Float -> WAdjMatrix
updateMatrix w i j val = take i w ++ [updateJ (w!!i) j val] ++ drop (i+1) w
-- Test: updateMatrix [[Just 0.0, Just 3.3, Just 4], [Just 2, Nothing, Just 2], [Just 6.5, Just 2]] 0 1 (Just 2.2)

floydLoopK :: WAdjMatrix -> Int -> Int -> [Int] -> WAdjMatrix
floydLoopK w i j [] = w
floydLoopK w i j l = updateMatrix w i j (minIJ [ minDis i j k w | k <- l ])

floydLoopJ :: WAdjMatrix -> Int -> [Int] -> WAdjMatrix
floydLoopJ w i [] = w
floydLoopJ w i (x:xs)= floydLoopK (floydLoopJ w i xs) i x [0..(length w -1)]

floydLoopI :: WAdjMatrix -> [Int] ->WAdjMatrix
floydLoopI w [] = w
floydLoopI w (x:xs)= floydLoopJ (floydLoopI w xs) x [0..(length w -1)]

floydWarshall :: WAdjMatrix -> WAdjMatrix
floydWarshall w =  if null w then [] else floydLoopI w [0..(length w -1)]
-- Test: wadjm = [[Just 0.0, Just 1.0, Just 2.0, Nothing, Nothing, Just 10.0], [Nothing, Just 0.0, Nothing, Just 5.0, Nothing, Nothing], [Nothing, Nothing, Just 0.0, Just 1.0, Just 1.0, Just 4.0], [Nothing, Nothing, Nothing, Just 0.0, Just 4.0, Nothing], [Nothing, Nothing, Nothing, Nothing, Just 0.0, Nothing], [Just 1.0, Nothing, Nothing, Nothing, Just 4.0, Just 0.0]]
-- Test: floydWarshall wadjm

-- Test: wadjm2 = [[Just 0.0, Just 1.0, Nothing, Just 2.0, Nothing], [Nothing, Just 0.0, Nothing, Nothing, Just 4.0], [Just 10.0, Nothing, Just 0.0, Just 5.0, Nothing], [Just 3.0, Just 9.0, Nothing, Just 0.0, Just 2.0], [Nothing, Just 6.0, Just 7.0, Nothing, Just 0.0]]
-- Test: floydWarshall wadjm2