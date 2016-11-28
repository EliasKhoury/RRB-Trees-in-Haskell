import qualified Data.Vector as V
import Control.Monad


branchingFactor :: Int
branchingFactor = 4 -- Branching factor of tree


------------------------------------------------------
--                                                  --
--                  TREE STRUCTURE                  --
--                                                  --
------------------------------------------------------

type Height = Int
type Sizes = [Int]

data Tree a = Node Height [Tree a] Sizes | Leaf [a]
    deriving (Show)

testTree :: Tree Int
testTree = buildTree [0..300]

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


ppTree :: Int -> Tree Int -> IO()
ppTree d (Leaf a)  = do
                        replicateM (d - 2) (putStr "        ") 
                        putStr "\t"
                        putStr "    ⊢---"
                        print a
ppTree d (Node n ts _) = do
                        replicateM (d - 2) (putStr "        ")
                        putStr "\t"
                        putStr "    ⊢---"
                        putStr "Node: "
                        print n
                        mapM (ppTree (d+1)) ts
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
relaxedSearch n (Leaf as) = as !! (n `mod` branchingFactor)
relaxedSearch i (Node l ts ss) = relaxedSearch (i - iOffset) (ts !! nodeCount) 
                where nodeCount = (i `div` (branchingFactor ^ l)) `mod` branchingFactor
                      iOffset   = (branchingFactor ^ l) * nodeCount


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


dropEndChild :: Tree a -> Int -> ([Tree a] -> Tree a) -> Tree a
dropEndChild (Leaf a) _ _ = Leaf a
dropEndChild (Node l ts ss) n f | l == n    = Node l (init ts) (if (length ss > 0) then init ss else [])
                                | otherwise = Node l ((init ts) ++ [dropRightChild (f ts) n]) ss

dropLeftChild :: Tree a -> Int -> Tree a
dropLeftChild tree l = dropEndChild tree l head

dropRightChild :: Tree a -> Int -> Tree a
dropRightChild tree l = dropEndChild tree l last

mergeEnds :: Tree a -> Tree a -> Tree a
mergeEnds t1 t2 = Node 1 [rightChild,leftChild] sizes
            where leftChild  = getLeftChild t2 0
                  rightChild = getRightChild t1 0
                  sizeLeft   = length ((\(Leaf a) -> a) leftChild)
                  sizeRight  = length ((\(Leaf a) -> a) rightChild)
                  sizes = if (sizeLeft /= branchingFactor || sizeRight /= branchingFactor) then [sizeRight,sizeLeft] else []


concatAtLevel :: (Tree a,Tree a,Tree a) -> (Tree a,Tree a,Tree a)
concatAtLevel = undefined