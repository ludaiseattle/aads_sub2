{-
  G54PAD Project in Advanced Algorithms and Data Structures
    Autumn 2022

  Assignment 3 
    Fibonacci Heaps

  Student Name: Lu Dai
  Student ID: 20350376

  Complete this Haskell file by providing definitions
  of the following functions:

  Operations on Wheels:
  readW, emptyW, isEmptyW, rightW, LeftW,
  insertW, extractW, concatW

  Operations on Fibonacci Heaps:
  emptyFH, isEmptyFH, minimumFH,
  insertFH, unionFH, extractFH

  You are allowed to define any other auxiliary function you need.

-}

module Fibonacci where

import Data.Map

-- WHEELS

-- Double linked circular lists
-- Implemented so all operations have amortized complexity O(1)

type Wheel a = ([a],[a])

{- A pair ([y1,..,yn],[z1,..,zm]) represents a circular lists
   with elements [y1,..,yn,zm,..,z1];
   the element to the right of z1 is y1, the element to the left of y1 is z1.
   The head element is y1
-}

-- reverse the list
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]
-- Test: reverseList []
-- Test: reverseList [1,2,3,4,5]

-- read the head element
readW :: Wheel a -> a
readW ([],[]) = error "empty list!"
readW ([], l2) = last l2
readW (l1,_) = head l1
-- Test: readW  ([1,2],[3,4])
-- Test: readW  ([],[3,4])
-- Test: readW  ([],[])

-- wheel containing no elements
emptyW :: Wheel a
emptyW = ([],[])
-- Test: emptyW

-- test if a wheel is empty 
isEmptyW :: Wheel a -> Bool
isEmptyW ([], []) = True
isEmptyW a = False
-- Test: isEmptyW ([],[])
-- Test: isEmptyW ([1],[])
-- Test: isEmptyW ([],[3])
-- Test: isEmptyW ([1],[2])


-- move the head to the next element clockwise
rightW :: Wheel a -> Wheel a
rightW ([],[]) = error "wheel is empty!"
rightW (x:xs, []) = (xs++[x], [])
rightW ([], xs:x) = ([], x++[xs])
rightW (x:xs, l2) = (xs, x:l2)
-- Test: rightW ([],[])
-- Test: rightW ([1,2,3],[])
-- Test: rightW ([], [3,2,1])
-- Test: rightW ([1,2,3], [6,5,4])

-- move the head to the next element anti-clockwise
leftW :: Wheel a -> Wheel a
leftW ([],[]) = error "wheel is empty!"
leftW (l1, []) = (last l1:init l1, [])
leftW ([], l2) = ([], tail l2++[head l2])
leftW (l1, l2) = (head l2:l1, tail l2)
-- Test: leftW ([],[])
-- Test: leftW ([1,2,3],[])
-- Test: leftW ([],[3,2,1])
-- Test: leftW ([1,2,3],[6,5,4])

-- insert a new element the the left of the head and set as new head
insertW :: a -> Wheel a -> Wheel a
insertW e (l1, l2) = (e:l1, l2)
-- Test: insertW 1 ([2,3],[6,5,4])
-- Test: insertW 1 ([],[])

-- extract and delete the head,  move the head to the next right
extractW :: Wheel a -> (a, Wheel a)
extractW ([],[]) = error "wheel is empty!"
extractW ([], l2) = (last l2, ([], init l2))
extractW (l1, l2) = (head l1, (tail l1, l2))
-- Test: extractW ([],[])
-- Test: extractW ([],[1])
-- Test: extractW ([],[1,2])
-- Test: extractW ([1,2,3],[6,5,4])
-- Test: extractW ([1,2,3],[])
-- Test: extractW ([3],[6,5,4]) 
-- needn't reverse the l2 when l1 is empty, because it is a wheel
-- when it is been used, just split it half and half

-- concatenate two wheels
--   the new head is the head of the first (if non-empty)
concatW :: Wheel a -> Wheel a -> Wheel a
concatW ([], []) (l21, l22) = (l21, l22)
concatW (l11, l12) ([], []) = (l11, l12)
concatW (l11, l12) (l21, l22) = split' (combineW (reSeq (l11, l12)) (reSeq (l21, l22))) 
-- Test: concatW ([1,2,3], []) ([], [6,5,4])
-- Test: concatW ([1,2,3], []) ([], [])
-- Test: concatW ([1,2,3], []) ([4,5,6], [])
-- Test: concatW ([], []) ([], [])
-- Test: concatW ([], []) ([1,2,3], [])

reSeq :: Wheel a -> Wheel a
reSeq (l1, l2) = (l1 ++ reverse l2, [])

combineW :: Wheel a -> Wheel a -> Wheel a 
combineW (l11, []) (l21, []) = (l11 ++ l21, [])

split' :: Wheel a -> Wheel a
split' (l1, []) = (Prelude.take (div (length l1) 2) l1, reverse (Prelude.drop (div (length l1) 2) l1))


-- FIBONACCI HEAPS
{-
  A FH is a min-ordered tree consisting of
  a wheel of nodes each having a subtree
  Each node contains its degree
  We also keep track of the number of elements of the heap
-}

data FibHeap a = FHeap Int (Wheel (FHNode a))
  deriving Show

-- FHeap n w: a heap with n elements, consisting of a root wheel w

type FHNode a = (a, Int, FibHeap a)

-- (x,d,h) is an element with value x, degree d and sub-heap h


-- the Fibonacci heap with no elements
emptyFH :: FibHeap a
emptyFH = FHeap 0 ([],[])
-- Test: emptyFH

-- test if a heap is empty
isEmptyFH :: FibHeap a -> Bool
isEmptyFH (FHeap 0 w) = True
isEmptyFH (FHeap num _) = if num < 0 then error "element number error!" else False
-- Test: isEmptyFH (FHeap 0 ([],[]))
-- Test: isEmptyFH (FHeap 2 ([],[]))
-- Test: isEmptyFH (FHeap -1 ([],[]))


-- Reading the minimum element
--  We assume that the head is heap-ordered,
--  so the minimum is the head of the root wheel
minimumFH :: FibHeap a -> a
minimumFH (FHeap 0 _) = error "the Fheap is empty"
minimumFH (FHeap _ ([], l2)) = first (last l2)
minimumFH (FHeap _ (l1, _)) = first (head l1)
-- Test: minimumFH (FHeap 0 [[],[]])
-- Test: minimumFH (FHeap 1 ([],[(999,2,emptyFH), (888, 2, emptyFH)]))
-- Test: minimumFH (FHeap 1 ([(999,2,emptyFH), (888, 2, emptyFH)],[]))
-- Test: minimumFH (FHeap 1 ([(999,2,emptyFH), (888, 2, emptyFH)],[(666,2,emptyFH), (777, 2, emptyFH)]))

first::(a,b,c) -> a
first (x, _, _) = x


-- Inserting a new element into the heap
insertFH :: Ord a => a -> FibHeap a -> FibHeap a
insertFH x h@(FHeap n w) =
  if (isEmptyW w) then FHeap 1 ([(x, 0, emptyFH)],[])
  else if x <= minimumFH h
    then FHeap (n+1) (insertW (x, 0, emptyFH) w)
    else FHeap (n+1) (rightW (insertW (x, 0, emptyFH) w))
--test: insertFH 1 (FHeap 2 ([(2,0,FHeap 0 ([],[]))],[(5,0,FHeap 0 ([],[]))]))
--test: insertFH 6 (FHeap 2 ([(2,0,FHeap 0 ([],[]))],[(5,0,FHeap 0 ([],[]))]))

-- Merging two Fibonacci Heaps
unionFH :: Ord a => FibHeap a -> FibHeap a -> FibHeap a
unionFH h1@(FHeap n1 w1) h2@(FHeap n2 w2) =
  if isEmptyFH h1 then h2 else
  if isEmptyFH h2 then h1 else
  if minimumFH h1 <= minimumFH h2
    then FHeap (n1+n2) (concatW w1 w2)
    else FHeap (n1+n2) (concatW w2 w1)
--Test: unionFH (FHeap 2 ([(2,0,FHeap 0 ([],[]))],[(5,0,FHeap 0 ([],[]))])) (FHeap 2 ([(2,0,FHeap 0 ([],[]))],[(5,0,FHeap 0 ([],[]))])) 
--Test: unionFH (FHeap 2 ([(6,0,FHeap 0 ([],[]))],[(5,0,FHeap 0 ([],[]))])) (FHeap 2 ([(2,0,FHeap 0 ([],[]))],[(5,0,FHeap 0 ([],[]))])) 


--data FibHeap a = FHeap Int (Wheel (FHNode a))
--  deriving Show

--type FHNode a = (a, Int, FibHeap a)

insNA :: (Num a, Ord a) => FHNode a -> Map Int (FHNode a) -> Map Int (FHNode a)
insNA x@(kx, dx, hx) map =
  if notMember dx map
    then insert dx x map  
    else insNA (link x (Data.Map.findWithDefault (0,0,FHeap 0 ([],[])) dx map)) (delete dx map)

link:: Ord a => FHNode a-> FHNode a-> FHNode a
link x@(kx, dx, hx@(FHeap nx wx)) y@(ky, dy, hy@(FHeap ny wy)) =
  if kx <= ky
  then (kx, dx+1, FHeap (ny+nx) (insertW y wx)) -- ny+nx ??????
  else (ky, dx+1, FHeap (ny+nx) (insertW x wy))
 
makeNA:: (Num a, Ord a) => (Wheel (FHNode a)) -> Map Int (FHNode a)
makeNA w = 
  if (isEmptyW w)
    then Data.Map.empty
    else let (x, w') = extractW w
    in insNA x (makeNA w')

{-
wheelNA :: Map Int (FHNode a) -> Wheel (FHNode a)
wheelNA map = 
  if Data.Map.null map 
  then emptyW
  else getWheel (elems map) emptyW 

getWheel :: [FHNode a] -> Wheel (FHNode a) -> Wheel (FHNode a)
getWheel [] w = w
getWheel (x:xs) w = getWheel xs (insNode x w)

insNode :: FHNode a -> Wheel (FHNode a) -> Wheel (FHNode a)
insNode x w = 
  if (isEmptyW w) or (x <= head w)
    then (insertW x w)
    else (rightW (insertW x w))

consolidate :: FibHeap a-> FibHeap a
consolidate (FHeap _ w) = wheelNA (makeNA w)
  
-- Extracting the minimum from a heap
extractFH :: Ord a => FibHeap a -> (a,FibHeap a)
extractFH (FHeap n w) = 
  let ((x, dx, FHeap nx wx), w') = extractW w
  in (x, dx, consolidate (FHeap (nx+(n-1)) (concatW wx w')))
-}