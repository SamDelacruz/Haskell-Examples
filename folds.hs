-- Folds take a binary function (such as + or div), a starting value (usually called an accumulator) and a list to 'fold up'
-- Lists can be folded from the left or right
-- The fold function calls the binary function with the accumulator and the first (or last) element of the list as parameters
-- The result is a new accumulator, which is used by the fold function on the next list element
-- This continues until the function has traversed the entire list, returning the final accumulator value

-- sum function using a left fold (foldl)
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs
-- Sum' explained:
    -- (\acc x -> acc + x) is the binary function - it takes two parameters, acc and x and sums them.
    -- When used in the context of foldl, acc is the current accumulator (initially set to 0), and the current head of the list (xs).
    -- foldl traverses the entire list xs, applying the binary function to a current accumulator and an element of xs

-- The sum function can be written more succintly - the binary function can be replaced by (+), and xs can be omitted to make this a curried function
sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0
-- We can omit xs as the parameter because calling foldl (+) 0 will return a function that takes a list
-- In general, with a function like:
    -- f a = g b a
-- You may rewrite as:
    -- f = g b
-- because of currying

-- map function using a right fold (foldr)
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs
-- We used a right fold here for a very important reason:
-- we are prepending values to a list (the accumulator), which is a fast operation
-- if we were building a new list using a foldl, we would have to use the (++) operator, which can be much slower
-- So foldr should be used when building a new list from an existing list

-- elem implementation using a right fold
elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldr (\x acc -> if x == y then True else acc) False ys

-- list reversal
reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

-- list product
product' :: (Num a) => [a] -> a
product' = foldl1 (*)

-- filter
filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

-- last
last' :: [a] -> a
last' = foldl1 (\_ x -> x)

-- and
and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs

-- Using scans to monitor progress of a fold!
sqrtSums :: Int
sqrtSums = length (takeWhile(<1000) (scanl1 (+) (map sqrt [1..]))) + 1

sqrtSums' :: [Double]
sqrtSums' = takeWhile(<1000) (scanl1 (+) (map sqrt [1..]))

-- function composition
oddSquareSum :: Integer
oddSquareSum = sum . takeWhile (<10000) . filter odd $ map (^2) [1..]