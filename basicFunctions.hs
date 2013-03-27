doubleMe :: Integer -> Integer
doubleMe x = 2*x

doubleList :: [Integer] -> [Integer]
doubleList xs = [2 * x | x <- xs]

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Pattern Matching!
head' :: [a] -> a
head' [] = error "Can't call head of an empty List!"
head' (x:_) = x

-- More pattern matching!
tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "The list is long, the first two elements are: " ++ show x ++ " and " ++ show y

-- As-patterns
firstLetter :: String -> String
firstLetter "" = "Empty String, whoops!"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- Guards - A bit like if expressions
	-- Check whether the property of a passed value is true or false
	-- If true, then function equals whatever you define it to be in that case
bmiTell :: Double -> String
bmiTell bmi
	| bmi <= 18.5 = "You're underweight!"
	| bmi <= 25.0 = "You're normal, apparently..."
	| bmi <= 30.0 = "You're a bit on the podgy side"
	| otherwise = "You're mahoooooosive!"
	
-- You can use Guards with functions with multiple parameters...
bmiTell' :: Double -> Double -> String
bmiTell' weight height
	| weight / height ^ 2 <= 18.5 = "You're underweight!"
	| weight / height ^ 2 <= 25.0 = "You're normal!"
	| weight / height ^ 2 <= 30.0 = "You're a bit overweight!"
	| otherwise = "Fatty"
	
-- Max function using guards
max' :: (Ord a) => a -> a -> a
max' a b
	| a <= b	= b
	| otherwise = a

-- Compare function using guards
myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
	| a == b 	= EQ
	| a <= b	= LT
	| otherwise = GT
	
-- Using 'where' to store intermediate values, improving bmiTell
bmiTellWhere :: Double -> Double -> String
bmiTellWhere weight height
	| bmi <= skinny = "You're underweight"
	| bmi <= normal = "You're normal"
	| bmi <= fat 	= "You're overweight"
	| otherwise 	= "You're huge"
	where 
		bmi = weight / height ^ 2
		(skinny, normal, fat) = (18.5, 25.0, 30.0)
		
-- pattern matching with where
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
	where
		(f:_) = firstname
		(l:_) = lastname
		
-- Functions in where Blocks
calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
	where
		bmi weight height = weight / height ^ 2
		
-- Basic let expression usage: let <bindings> in <expression>.
-- You can use let expressions almost anywhere, as with any expression
cylinder :: Double -> Double -> Double
cylinder r h =
	let
		sideArea = 2 * pi * r * h
		topArea = pi * r^2
	in
		sideArea + 2 * topArea
	
-- Let in List Comprehensions

calcBmis' :: [(Double, Double)] -> [Double]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi > 25.0]

-- Basic case expression usage:
head'' :: [a] -> a
head'' xs = case xs of [] -> error "No head for empty lists!"
                       (x:_) -> x
					   
-- inline pattern matching with case expressions
describeList :: [a] -> String
describeList ls = "The list is " ++ case ls of
	[] -> "empty."
	[x] -> "a singleton list."
	xs -> "a longer list."
	
-- more pattern matching

describeList' :: [a] -> String
describeList' ls = "The list is " ++ what ls
	where
		what [] = "empty."
		what [x] = "a singleton list."
		what xs = "a longer list."