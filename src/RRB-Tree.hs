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
ppTree' d (Node n ts _) = do
                        replicateM (d - 2) (putStr "        ")
                        putStr "\t"
                        putStr "    ⊢---"
                        putStr "Node: "
                        print n
                        mapM (ppTree' (d+1)) ts
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


dropLeftChild :: Tree a -> Int -> Tree a
dropLeftChild (Leaf a) _ = Leaf a
dropLeftChild (Node l ts ss) n | l == n    = Node l (tail ts) (if (length ss > 0) then tail ss else [])
                               | otherwise = Node l ([dropLeftChild (head ts) n] ++ tail ts) ss 

dropRightChild :: Tree a -> Int -> Tree a
dropRightChild (Leaf a) _ = Leaf a
dropRightChild (Node l ts ss) n | l == n    = Node l (init ts) (if (length ss > 0) then init ss else [])
                                | otherwise = Node l (init ts ++ [dropRightChild (last ts) n]) ss 

mergeEnds :: Tree a -> Tree a -> Tree a
mergeEnds t1 t2 = Node 1 [Leaf leftChild, Leaf rightChild] sizes
            where Leaf leftLeaves  = getRightChild t1 0
                  Leaf rightLeaves = getLeftChild t2 0
                  leaves = leftLeaves ++ rightLeaves
                  leftChild = take branchingFactor leaves
                  rightChild = drop branchingFactor leaves
                  sizes = if (length leftChild) /= (length rightChild) then [length leftChild,length rightChild] else []

stripTree :: Tree a -> [Tree a]
stripTree (Leaf a) = [Leaf a]
stripTree (Node _ ts _) = ts

balanceLeaves :: [Tree a] -> [Tree a]
balanceLeaves ts = buildLeaves as
                where as = concat $ map (\(Leaf a) -> a) ts

mergeNodes :: [Tree a] -> [Tree a]
mergeNodes ns = case head firstMerge of
                  (Leaf _)     -> balanceLeaves secondMerge
                  (Node l _ _) -> map (\ts -> Node l ts []) (collect branchingFactor secondMerge) -- GET SIZES
                where firstMerge  = concat $ map stripTree ns
                      secondMerge = concat $ map stripTree firstMerge

snake :: Tree Int
snake = mergeRebalance (t1,t2,t3)
      where t1 = getRightChild (dropRightChild oneto10 1) 1
            t2 = mergeEnds oneto10 tento20
            t3 = getLeftChild (dropLeftChild tento20 1) 1

mergeRebalance :: (Tree a,Tree a,Tree a) -> Tree a
mergeRebalance (t1,t2,t3) = Node (l + 1) [left,right] [] -- FIND SIZES
                        where mergedNodes = mergeNodes[t1,t2,t3]
                              Node l _ _ = t2
                              left = Node l (take branchingFactor mergedNodes) [] -- FIND SIZES
                              right = Node l (drop branchingFactor mergedNodes) []