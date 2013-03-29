{--------------------------------------
 --- Solutions to Exercise Sheet W3 ---
 --------------------------------------}

mid :: (a, b, c) -> b
mid (x, y, z) = y

ordered :: Ord a => (a, a, a) -> Bool
ordered (x, y, z) = x <= y && y <= z

addup3things :: Num a => a -> a -> a -> a
addup3things x y z = x + y + z

f :: Eq a => a -> a -> Bool
f x = \y -> x == y

g :: [(a, b, c)] -> [b]
g xs = map mid xs

split :: (a -> Bool) -> ([a] -> ([a], [a]))
split p xs = ([ x | x <- xs , p x],[ y | y <- xs , not $ p y ])

partition :: Ord a => a -> [a] -> ([a] , [a])
partition y = split (< y)


safemaximum :: Ord a => [a] -> Maybe a
safemaximum [] = Nothing
safemaximum xs = Just (maximum xs)

find :: (a -> Bool) -> [a] -> Maybe a
find p [] = Nothing
find p (x:xs) = case p x of
    True -> Just x
    False -> find p xs
    
madd :: Num a => Maybe a -> Maybe a -> a
madd Nothing Nothing = 0
madd (Just x) Nothing = x
madd Nothing (Just y) = y
madd (Just x) (Just y) = x + y