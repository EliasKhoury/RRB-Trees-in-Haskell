import qualified Data.Vector as V
import Control.Monad


branchingFactor :: Int
branchingFactor = 32 -- Branching factor of tree


------------------------------------------------------
--                                                  --
--                  TREE STRUCTURE                  --
--                                                  --
------------------------------------------------------

type Height = Int
type Sizes = [Int]

data Tree a = Node Height [Tree a] Sizes | Leaf [a]
    deriving (Show)

testTree :: Int -> Tree Int
testTree n = buildTree [0..n]

oneto10 :: Tree Int
oneto10 = buildTree[0..150]

tento20 :: Tree Int
tento20 = buildTree[151..300]

buildTree :: [a] -> Tree a
buildTree as = buildNodes leaves 1
        where leaves = collect branchingFactor (buildLeaves as)

buildNodes :: [[Tree a]] -> Int -> Tree a
buildNodes [as] n = Node n as []
buildNodes ass n = buildNodes (collect branchingFactor elems) (n+1)
            where elems = map (\as -> Node n as []) ass

buildLeaves :: [a] -> [Tree a]
buildLeaves as = map (\a -> Leaf a) (collect branchingFactor as)

collect :: Int -> [a] -> [[a]]
collect _ [] = []
collect n as = take n as : collect n (drop n as)

------------------------------------------------------
--                                                  --
--                  PRETTY PRINTER                  --
--                                                  --
------------------------------------------------------

ppTree t = ppTree' level t
        where Node level _ _ = t

ppTree' :: Int -> Tree Int -> IO()
ppTree' d (Leaf a)  = do
                        replicateM (d - 2) (putStr "        ") 
                        putStr "\t"
                        putStr "    ⊢---"
                        print a
ppTree' d x (Node n ts ss) = do
                        replicateM (d - x) (putStr "        ")
                        putStr "\t"
                        putStr "    ⊢---"
                        putStr "Node: "
                        print n
                        replicateM (d - x) (putStr "        ")
                        putStr "\t"
                        putStr "    Sizes: "
                        print ss
                        mapM (ppTree' d (x-1)) ts
                        return ()



------------------------------------------------------
--                                                  --
--                  LOOKUP FUNCTION                 --
--                                                  --
------------------------------------------------------

-- Given an index, the lookup function returns the leaf at that index
radixSearch :: Int -> Tree a -> a 
radixSearch n (Leaf as) = as !! (n `mod` branchingFactor)
radixSearch i (Node l ts ss) = radixSearch (i - iOffset) (ts !! nodeCount) 
                where nodeCount = (i `div` (branchingFactor ^ l)) `mod` branchingFactor
                      iOffset   = (branchingFactor ^ l) * nodeCount


relaxedSearch :: Int -> Tree a -> a 
relaxedSearch i (Leaf as) = as !! (i `mod` branchingFactor)
relaxedSearch i (Node l ts [])  = relaxedSearch (i - iOffset) (ts !! nodeCount) 
                where nodeCount = (i `div` (branchingFactor ^ l)) `mod` branchingFactor
                      iOffset   = (branchingFactor ^ l) * nodeCount
relaxedSearch i (Node l ts ss)  = relaxedSearch (i - iOffset) (ts !! subTree)
                where iOffset   = if subTree == 0 then 0 else ss !! (subTree - 1) 
                      subTree   = length (takeWhile (< i) ss) 


------------------------------------------------------
--                                                  --
--                  CONCAT FUNCTION                 --
--                                                  --
------------------------------------------------------

data Choice = Left | Right

getEndChild :: Tree a -> Int -> ([Tree a] -> Tree a) -> Tree a
getEndChild (Leaf a) _ _ = Leaf a
getEndChild (Node l ts ss) n f | l == n    = Node l ts ss
                               | otherwise = getEndChild (f ts) n f

getLeftChild :: Tree a -> Int -> Tree a
getLeftChild tree l = getEndChild tree l head

getRightChild :: Tree a -> Int -> Tree a
getRightChild tree l = getEndChild tree l last

dropLeftChild :: Tree a -> Int -> Tree a
dropLeftChild (Node l ts _) n = if singleKid || l == (n + 1) then Node l (tail ts) [] else Node l ([dropLeftChild (head ts) n] ++ tail ts) []
                  where singleKid = length (getKids (head ts)) == 1

dropRightChild :: Tree a -> Int -> Tree a
dropRightChild (Node l ts _) n = if singleKid || l == (n + 1) then Node l (init ts) [] else Node l (init ts ++ [dropRightChild (last ts) n]) []
                  where singleKid = length (getKids (last ts)) == 1

{-
dropLeftChild :: Tree a -> Int -> Tree a
dropLeftChild (Leaf a) _ = Leaf a
dropLeftChild (Node l ts ss) n | l == n    = Node l (tail ts) (if (length ss > 0) then tail ss else [])
                               | otherwise = Node l ([dropLeftChild (head ts) n] ++ tail ts) ss 

dropRightChild :: Tree a -> Int -> Tree a
dropRightChild (Leaf a) _ = Leaf a
dropRightChild (Node l ts ss) n | l == n    = Node l (init ts) (if (length ss > 0) then init ss else [])
                                | otherwise = Node l (init ts ++ [dropRightChild (last ts) n]) ss 
-}

-- Joins the ends of two trees to start the merging process

-- Extracts children from a tree
getKids :: Tree a -> [Tree a]
getKids (Leaf a) = [Leaf a]
getKids (Node _ ts _) = ts

getSizes :: Tree a -> [Int]
getSizes (Leaf a)      = [length a]
getSizes (Node _ _ ss) = ss

-- Joins the ends of two trees to make a new tree of height 2
mergeEnds :: Tree a -> Tree a -> Tree a
mergeEnds t1 t2 = Node 2 [Node 1 leftChild [], Node 1 rightChild []] (if lSize == rSize then [] else [lSize,rSize])
            where Node _ leftLeaves _  = getRightChild t1 1
                  Node _ rightLeaves _ = getLeftChild t2 1
                  leaves = leftLeaves ++ rightLeaves
                  leftChild = take branchingFactor leaves
                  rightChild = drop branchingFactor leaves
                  [lSize,rSize] = map (length.stripLeaf) [last leftChild,last rightChild]
                  stripLeaf = \(Leaf a) -> a

-- Rebuilds leaves to maintain branching factor of m
balanceLeaves :: [Tree a] -> [Tree a]
balanceLeaves ts = buildLeaves as
                where as = concat $ map (\(Leaf a) -> a) ts

-- Rebuilds a list of nodes by taking all their children and reconstructing the parents
mergeNodes :: [Tree a] -> [Tree a]
mergeNodes ns = case head firstMerge of
                  (Leaf _)     -> balanceLeaves secondMerge
                  (Node l _ _) -> map (\ts -> Node l ts (computeSizes ts l)) (collect branchingFactor secondMerge) -- GET SIZES
                where firstMerge  = concat $ map getKids ns
                      secondMerge = concat $ map getKids firstMerge

computeSizes :: [Tree a] -> Int -> [Int]
computeSizes ts l = if all (==[]) sizes then [] else calcNewSize sizes l 0
                        where sizes = map getSizes ts

calcNewSize :: [[Int]] -> Int -> Int -> [Int]
calcNewSize [] _ _      = []
calcNewSize ([]:ss) l i = cumulativeIndex : (calcNewSize ss l cumulativeIndex) 
                  where cumulativeIndex = (branchingFactor ^ l) + i
calcNewSize (s:ss) l i = cumulativeIndex : (calcNewSize ss l cumulativeIndex)
                  where cumulativeIndex = (last s) + i


mergeRebalance :: (Tree a,Tree a,Tree a) -> Tree a
mergeRebalance (t1,t2,t3) = Node (l + 1) [left,right] (computeSizes [left,right] (l+1)) -- FIND SIZES
                        where mergedNodes = mergeNodes[t1,t2,t3]
                              Node l _ _  = t1
                              leftChildren = (take branchingFactor mergedNodes)
                              rightChildren = (drop branchingFactor mergedNodes)
                              left = Node l leftChildren (computeSizes leftChildren l) -- FIND SIZES
                              right = Node l rightChildren (computeSizes rightChildren l)


-- Merges left and right tree into the middle tree at a given height
mergeAt :: Int -> (Tree a,Tree a,Tree a) -> (Tree a,Tree a,Tree a)
mergeAt n (t1,t2,t3) = (leftTree,mid,rightTree)
                where leftTree   = dropRightChild t1 n
                      leftChild  = getRightChild t1 n
                      mid        = mergeRebalance (leftChild,t2,rightChild)
                      rightTree  = dropLeftChild t3 n
                      rightChild = getLeftChild t3 n


badger :: Tree Int
badger = t2''
      where t1 = dropRightChild oneto10 1
            t2 = mergeEnds oneto10 tento20
            t3 = dropLeftChild tento20 1
            (t1',t2',t3') =  mergeAt 2 (t1,t2,t3)
            (t1'',t2'',t3'') =  mergeAt 3 (t1',t2',t3')
            --(_,middleTree,_) =  mergeAt 4 (t1'',t2'',t3'')
