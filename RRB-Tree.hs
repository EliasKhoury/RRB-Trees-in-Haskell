import qualified Data.Vector as V



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
testTree = buildTree [0..120]

buildTree :: [a] -> Tree a
buildTree as = buildNodes leaves 0
        where leaves = collect (buildLeaves as) branchingFactor

buildNodes :: [[Tree a]] -> Int -> Tree a
buildNodes [as] n = Node n as []
buildNodes ass n = buildNodes (collect elems branchingFactor) (n+1)
            where elems = map (\as -> Node n as []) ass

buildLeaves :: [a] -> [Tree a]
buildLeaves as = map (\a -> Leaf a) as

collect :: [a] -> Int -> [[a]]
collect [] _ = []
collect as n = take n as : collect (drop n as) n

