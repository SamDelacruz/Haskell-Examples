myMap :: (a -> b) -> [a] -> [b]
myMap g = foldr f z
    where   f x y =  (g x) : y
            z = []
            
myConcat :: [[a]] -> [a]
myConcat = foldr f z
    where   f x y = x ++ y
            z = []
            
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter g = foldr f z
    where   f x y = case (g x) of
                True -> x : y
                False -> y
            z = []
            
myFind :: (a -> Bool) -> [a] -> Maybe a
myFind g = foldr f z
    where   f x y = case (g x) of
                True -> Just x
                False -> y
            z = Nothing
            
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl g acc xs = foldr (flip g) acc (reverse xs)

data Tree a = Leaf a | Tree a :+: Tree a deriving Show
test = myFoldl (:+:) (Leaf 0) [Leaf n | n <- [1..5]]