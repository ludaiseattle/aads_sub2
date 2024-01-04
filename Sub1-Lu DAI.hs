{-
  COMP4040 Project in Advanced Algorithms and Data Structures
    Autumn 2022

  Assignment 1 
     Red-Balck Trees

  Student Name: Lu DAI
  Student ID: 20350376

  Complete this Haskell file by providing definitions
  of the following functions (do not change their types):

    searchRB

    minRB
    maxRB

    isBST

    blackBalanced
    blackHeight

    insertRB
    deleteRB

  You are allowed to define any other auxiliary function you need.

-}

module RedBlack where

-- Definition of the Red-Black Tree data structure

data Color = Red | Black
  deriving (Eq,Show)

data RBT a = LeafRB | NodeRB Color (RBT a) a (RBT a)
  deriving (Eq,Show)

-- Serching a key inside a red-black tree
--   return True if the key is found, False otherwise

searchRB :: Ord a => a -> RBT a -> Bool
searchRB _ LeafRB = False
searchRB x (NodeRB _ left a right)
  | x == a = True
  | x < a = searchRB x left
  | x > a = searchRB x right

-- Minimum and maximum of red-black tree
--   return Nothing if the tree is empty

minRB :: RBT a -> Maybe a
minRB LeafRB = Nothing
minRB (NodeRB _ LeafRB a _) = Just a
minRB (NodeRB _ left _ _) = minRB left

maxRB :: RBT a -> Maybe a
maxRB LeafRB = Nothing
maxRB (NodeRB _ _ a LeafRB) = Just a
maxRB (NodeRB _ _ _ right) = maxRB right
  

-- inorder of the tree
inorder :: RBT a -> [a]
inorder t = case t of
    LeafRB -> []
    NodeRB _ left a right -> (inorder left) ++ [a] ++ (inorder right)

sort' :: (Ord a) => [a] -> [a]
sort' [] = []
sort' (x:xs) = 
    let min = sort' (filter (<x) xs)
        max = sort' (filter (>x) xs)
    in min ++ [x] ++ max

-- Check if a tree satisfies the Binary Search Tree condition
--   (do not check other RBT conditions)
isBST :: Ord a => RBT a -> Bool
isBST LeafRB = False
isBST bst@(NodeRB color left a right) = (inorder bst == sort'(inorder bst))

hightInOneRoad :: RBT a -> Int
hightInOneRoad LeafRB = 0
hightInOneRoad (NodeRB Black left _ _) = 
  1 + hightInOneRoad left
hightInOneRoad (NodeRB Red left _ _) = 
  hightInOneRoad left


-- Check the Black-balancing condition:
--     all paths have the same number of black nodes

blackBalanced :: RBT a -> Bool
blackBalanced LeafRB = True
blackBalanced (NodeRB color left _ right) = 
  blackBalanced left 
  && blackBalanced right 
  && isSameHight (hightInOneRoad left) (hightInOneRoad right)
  where
    isSameHight a b   
      | a == b = True
      | otherwise = False

-- Black height of a black-balanced tree, -1 if not black-balanced
blackHeight :: RBT a -> Int
blackHeight a =
  if not (blackBalanced a)
    then -1 
  else
    hightInOneRoad a

-- Check if all Red-Black Tree conditions are satisfied
isRBT :: Ord a => RBT a -> Bool
isRBT a = isBST a && blackBalanced a

-- Insert a new element in a RBT, preserving the RBT properties

insertRB :: Ord a => a -> RBT a -> RBT a
insertRB ele tree = chg2Black (insert tree)
  where insert LeafRB = NodeRB Red LeafRB ele LeafRB
        insert (NodeRB color left a right)
          | ele < a = reBalance color (insert left) a right 
          | ele == a = NodeRB color left a right
          | ele > a = reBalance color left a (insert right)
        chg2Black (NodeRB _ left a right) = NodeRB Black left a right

reBalance :: Color -> RBT a -> a -> RBT a -> RBT a
reBalance Black (NodeRB Red (NodeRB Red l3 a3 r3) a2 r2) a1 r1
  = NodeRB Red (NodeRB Black l3 a3 r3) a2 (NodeRB Black r2 a1 r1)
reBalance Black (NodeRB Red l2 a2 (NodeRB Red l3 a3 r3)) a1 r1  
  = NodeRB Red (NodeRB Black l2 a2 l3) a3 (NodeRB Black r3 a1 r1)
reBalance Black l1 a1 (NodeRB Red (NodeRB Red l3 a3 r3) a2 r2)  
  = NodeRB Red (NodeRB Black l1 a1 l3) a3 (NodeRB Black r3 a2 r2)
reBalance Black l1 a1 (NodeRB Red l2 a2 (NodeRB Red l3 a3 r3))  
  = NodeRB Red (NodeRB Black l1 a1 l2) a2 (NodeRB Black l3 a3 r3)
reBalance color l a r = NodeRB color l a r

-- Delete an element from a RBT, preserving the RBT properties

deleteRB :: Ord a => a -> RBT a -> RBT a
deleteRB ele  = chg2Black . delete 
  where
    delete LeafRB = LeafRB
    delete t@(NodeRB _ l a r) 
        | ele < a = delL t
        | ele > a = delR t
        | otherwise = fuse l r
    delL (NodeRB _ l@(NodeRB Black _ _ _) a r) = reBalanceL (NodeRB Black (delete l) a r)
    delL (NodeRB _ l a r) = NodeRB Red (delete l) a r
    delR (NodeRB _ l a r@(NodeRB Black _ _ _)) = reBalanceR (NodeRB Black l a (delete r))
    delR (NodeRB _ l a r) = NodeRB Red l a (delete r)
    chg2Black (NodeRB _ left a right) = NodeRB Black left a right

chg2Red :: RBT a -> RBT a
chg2Red LeafRB = LeafRB
chg2Red (NodeRB _ l a r) = NodeRB Red l a r

reBalanceL :: RBT a -> RBT a 
reBalanceL (NodeRB Black (NodeRB Red l2 a2 r2) a1 r1) = NodeRB Red (NodeRB Black l2 a2 r2) a1 r1
reBalanceL (NodeRB Black l1 a1 (NodeRB Black l2 a2 r2)) = reBalance Black l1 a1 (NodeRB Red l2 a2 r2)
reBalanceL (NodeRB Black l1 a1 (NodeRB Red (NodeRB Black l3 a3 r3) a2 r2)) = NodeRB Red (NodeRB Black l1 a1 l3) a3 (reBalance Black r3 a2 (chg2Red r2))

reBalanceR :: RBT a -> RBT a 
reBalanceR (NodeRB Black l1 a1 (NodeRB Red l2 a2 r2)) = NodeRB Red l1 a1 (NodeRB Black l2 a2 r2)
reBalanceR (NodeRB Black (NodeRB Black l2 a2 r2) a1 r1) = reBalance Black (NodeRB Red l2 a2 r2) a1 r1
reBalanceR (NodeRB Black (NodeRB Red l1 a1 (NodeRB Black l3 a3 r3)) a2 r2) = NodeRB Red (reBalance Black (chg2Red l1) a1 l3) a3 (NodeRB Black r3 a2 r2)

fuse :: RBT a -> RBT a -> RBT a
fuse LeafRB t2 = t2
fuse t1 LeafRB = t1 
fuse t1@(NodeRB Black _ _ _) (NodeRB Red l2 a2 r2) = NodeRB Red (fuse t1 l2) a2 r2
fuse (NodeRB Red l1 a1 r1) t2@(NodeRB Black _ _ _) = NodeRB Red l1 a1 (fuse r1 t2)
fuse (NodeRB Red l1 a1 r1) (NodeRB Red l2 a2 r2)  =
  let tmpT = fuse r1 l2
  in case tmpT of
       (NodeRB Red tmpL tmpA tmpR) -> (NodeRB Red (NodeRB Red l1 a1 tmpL) tmpA (NodeRB Red tmpR a2 r2))
       (NodeRB Black _ _ _)   -> (NodeRB Red l1 a1 (NodeRB Red tmpT a2 r2))
fuse (NodeRB Black l1 a1 r1) (NodeRB Black l2 a2 r2)  =
  let tmpT = fuse r1 l2
  in case tmpT of
       (NodeRB Red tmpL tmpA tmpR) -> (NodeRB Red (NodeRB Black l1 a1 tmpL) tmpA (NodeRB Black tmpR a2 r2))

