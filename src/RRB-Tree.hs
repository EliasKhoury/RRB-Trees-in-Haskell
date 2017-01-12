import Data.Ord
import Data.Time.Clock
import System.IO 
import System.Random
import System.Environment
import Control.Monad
import Control.DeepSeq

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
oneto10 = buildTree[0..30000]

tento20 :: Tree Int
tento20 = buildTree[30001..40000]

buildTree :: [a] -> Tree a
buildTree as = buildNodes leaves 1
        where leaves = collect branchingFactor (buildLeaves as)

buildNodes :: [[Tree a]] -> Int -> Tree a
buildNodes [as] n = Node n as []
buildNodes ass n = buildNodes (collect branchingFactor elems) (n+1)
            where elems = map (\as -> Node n as (computeSizes as n)) ass

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

ppTree t = ppTree' level level t
        where Node level _ _ = t

ppTree' :: (Show a) => Int -> Int -> Tree a -> IO()
ppTree' d x (Leaf a)  = do
                        replicateM (d - x) (putStr "        ") 
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
--                  INDEX FUNCTION                  --
--                                                  --
------------------------------------------------------



-- Given an index, the lookup function returns the leaf at that index
radixSearch :: Int -> Tree a -> a 
radixSearch i (Leaf as) = as !! (i `mod` branchingFactor)
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
--                 TAKE AND DROP                    --
--                                                  --
------------------------------------------------------

getEndChild :: Tree a -> Int -> ([Tree a] -> Tree a) -> Tree a
getEndChild (Leaf a) _ _       = Leaf a
getEndChild (Node l [] ss) n f = Node l [] ss
getEndChild (Node l ts ss) n f | l == n    = Node l ts ss
                               | otherwise = getEndChild (f ts) n f

getLeftChild :: Tree a -> Int -> Tree a
getLeftChild tree l = getEndChild tree l head

getRightChild :: Tree a -> Int -> Tree a
getRightChild tree l = getEndChild tree l last

dropLeftChild :: Tree a -> Int -> Tree a
dropLeftChild (Node l [] _) _ = Node l [] []
dropLeftChild (Node l ts _) n | l == n = Node l [] []
                              | otherwise = Node l ([dropLeftChild (head ts) n] ++ tail ts) []

dropRightChild :: Tree a -> Int -> Tree a
dropRightChild (Node l [] _) _ = Node l [] []
dropRightChild (Node l ts _) n | l == n = Node l [] []
                               | otherwise = Node l (init ts ++ [dropRightChild (last ts) n]) []         


{-

dropRightChild :: Tree a -> Int -> Tree a
dropRightChild (Node l ts _) n = if singleKid || l == (n + 1) then Node l (init ts) [] else Node l (init ts ++ [dropRightChild (last ts) n]) []
                  where singleKid = length (getKids (last ts)) == 1

dropLeftChild :: Tree a -> Int -> Tree a
dropLeftChild (Node l ts _) n = if singleKid || l == (n + 1) then Node l (tail ts) [] else Node l ([dropLeftChild (head ts) n] ++ tail ts) []
                  where singleKid = length (getKids (head ts)) == 1

dropLeftChild :: Tree a -> Int -> Tree a
dropLeftChild (Leaf a) _ = Leaf a
dropLeftChild (Node l ts ss) n | l == n    = Node l (tail ts) (if (length ss > 0) then tail ss else [])
                               | otherwise = Node l ([dropLeftChild (head ts) n] ++ tail ts) ss 

dropRightChild :: Tree a -> Int -> Tree a
dropRightChild (Leaf a) _ = Leaf a
dropRightChild (Node l ts ss) n | l == n    = Node l (init ts) (if (length ss > 0) then init ss else [])
                                | otherwise = Node l (init ts ++ [dropRightChild (last ts) n]) ss 
-}


------------------------------------------------------
--                                                  --
--                  CONCAT FUNCTION                 --
--                                                  --
------------------------------------------------------

-- Extracts children from a tree
getKids :: Tree a -> [Tree a]
getKids (Leaf a) = [Leaf a]
getKids (Node _ ts _) = ts

getSizes :: Tree a -> [Int]
getSizes (Leaf a)      = if length a < branchingFactor then [length a] else []
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
mergeNodes ns = case firstMerge of
                  []                 -> []
                  ((Leaf _):ns)      -> balanceLeaves secondMerge
                  ((Node l _ _):ns)  -> map (\ts -> Node l ts (computeSizes ts l)) (collect branchingFactor secondMerge)
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
mergeRebalance (t1,t2,t3) = Node (l + 1) [left,right] (computeSizes [left,right] (l+1))
                        where mergedNodes = mergeNodes[t1,t2,t3]
                              Node l _ _  = t1
                              leftChildren = (take branchingFactor mergedNodes)
                              rightChildren = (drop branchingFactor mergedNodes)
                              left = Node l leftChildren (computeSizes leftChildren l)
                              right = Node l rightChildren (computeSizes rightChildren l)


-- Merges left and right tree into the middle tree at a given height
mergeAt :: Int -> (Tree a,Tree a,Tree a) -> (Tree a,Tree a,Tree a)
mergeAt n (t1,t2,t3) = (leftTree,mid,rightTree)
                where leftTree   = dropRightChild t1 n
                      leftChild  = getRightChild t1 n
                      mid        = mergeRebalance (leftChild,t2,rightChild)
                      rightTree  = dropLeftChild t3 n
                      rightChild = getLeftChild t3 n

rrbConcat :: Tree a -> Tree a -> Tree a
rrbConcat t1@(Node l1 _ _) t2@(Node l2 _ _) = head ts
        where maxHeight = max l1 l2
              left      = dropRightChild t1 1
              middle    = mergeEnds t1 t2
              right     = dropLeftChild t2 1
              (_,Node _ ts _,_) = rrbConcat' maxHeight 2 (left,middle,right)

rrbConcat' :: Int -> Int -> (Tree a, Tree a, Tree a) -> (Tree a, Tree a, Tree a)
rrbConcat' maxl l ts = case maxl == l of 
                       True  -> newTrees
                       False -> rrbConcat' maxl (l+1) newTrees
                    where newTrees = mergeAt l ts


------------------------------------------------------
--                                                  --
--                  MAIN FUNCTION                   --
--                                                  --
------------------------------------------------------

flatten :: Tree a -> [a]
flatten (Leaf as) = as
flatten (Node _ ts _) = concat $ map flatten ts

genFileNames :: [String]
genFileNames = take 10 (map ("../data/testing-data/file"++) letters)
        where letters = [(a:b:[]) | a <- ['a'..'z'], b <- ['a'..'z']]

treeAppend :: Tree Char -> String -> IO(Tree Char)
treeAppend tree1 s = do 
            data1 <- readFile s
            let tree2 = buildTree data1
            let final = rrbConcat tree1 tree2
            t1 <- getCurrentTime
            t2 <- (getSizes final) `deepseq` getCurrentTime
            print (diffUTCTime t2 t1)
            g <- newStdGen
            let (index,_) = randomR (0,head (getSizes final)) g
            t1 <- getCurrentTime
            t2 <- (relaxedSearch index final) `deepseq` getCurrentTime
            print (diffUTCTime t2 t1)
            return final

joinFiles :: [String] -> Tree Char -> IO (Tree Char)
joinFiles [] t = return (t)
joinFiles (x:xs) t = do
                  tree <- treeAppend t x
                  joinFiles xs tree

main :: IO()
main = do 
       [data1,data2] <- mapM readFile (take 2 genFileNames)
       let tree1 = buildTree data1
       let tree2 = buildTree data2
       let joined = rrbConcat tree1 tree2
       t1 <- getCurrentTime
       t2 <- (getSizes joined) `deepseq` getCurrentTime
       print (diffUTCTime t2 t1)
       final <- joinFiles (drop 2 genFileNames) joined
       writeFile "tables.html" (flatten final)
       return ()
       --ppTree final