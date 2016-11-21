import qualified Data.Vector as V


branchingFactor :: Int
branchingFactor = 4 -- Branching factor of tree


------------------------------------------------------
--                                                  --
--                  TREE STRUCTURE                  --
--                                                  --
------------------------------------------------------

type Height = Int
type Sizes = [Int]

data Tree a = Node Height [Tree a] Sizes | Leaf a
    deriving (Show)

testTree :: Tree Int
testTree = buildTree [0..300]

buildTree :: [a] -> Tree a
buildTree as = buildNodes leaves 0
        where leaves = collect branchingFactor (buildLeaves as)

buildNodes :: [[Tree a]] -> Int -> Tree a
buildNodes [as] n = Node n as []
buildNodes ass n = buildNodes (collect branchingFactor elems) (n+1)
            where elems = map (\as -> Node n as []) ass

buildLeaves :: [a] -> [Tree a]
buildLeaves as = map (\a -> Leaf a) as

collect :: Int -> [a] -> [[a]]
collect _ [] = []
collect n as = take n as : collect n (drop n as)


------------------------------------------------------
--                                                  --
--                  LOOKUP FUNCTION                 --
--                                                  --
------------------------------------------------------

-- Given an index, the lookup function returns the leaf at that index
radixSearch :: Int -> Tree a -> a 
radixSearch _ (Leaf a) = a
radixSearch i (Node l ts ss) = radixSearch (i - iOffset) (ts !! nodeCount) 
                where nodeCount = (i `div` (branchingFactor ^ l)) `mod` branchingFactor
                      iOffset   = (branchingFactor ^ l) * nodeCount