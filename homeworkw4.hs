mySum :: Num a => [a] -> a
mySum [] = 0
mySum [x] = x
mySum (x:xs) = x + sum xs

myMinimum :: Ord a => [a] -> a
myMinimum [] = error "Cannot find minimum of an empty list"
myMinimum [x] = x
myMinimum (x:xs) = min x (minimum xs)
-- Function raises an exception if given an empty List,
-- Minimum of empty List makes no sense

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

tree :: Tree Integer
tree = Node 3 (Node 1 EmptyTree (Node 2 EmptyTree EmptyTree)) (Node 4 EmptyTree EmptyTree)

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f EmptyTree = EmptyTree
treeMap f (Node x leftTree rightTree) = Node (f x) (treeMap f leftTree) (treeMap f rightTree)
-- Result of treeMap (\x -> x+3) tree is:
    -- Node 6 (Node 4 EmptyTree (Node 5 EmptyTree EmptyTree)) (Node 7 EmptyTree EmptyTree)

height :: Tree a -> Int
height EmptyTree = 0
height ( Node _ leftTree rightTree) = 1 + max (height leftTree) (height rightTree)
-- Result of height tree: 3

flatten :: Tree a -> [a]
flatten EmptyTree = []
flatten ( Node x leftTree rightTree ) = (flatten leftTree) ++ [x] ++ (flatten rightTree)
-- Result of flatten tree: [1,2,3,4]