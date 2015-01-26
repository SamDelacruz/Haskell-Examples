-- Question 1
    --(a)
    third :: (a, b, c) -> c
    third (x, y, z) = z
    --(b)
    ordered :: Ord a => (a, a, a, a) -> Bool
    ordered (x, y, z, t) = x <= y && y <= z && z <= t
    --(c)
    multiply3things :: Num a => a -> a -> a -> a
    multiply3things x y z = x * y * z
    --(d)
    f :: Integral a => a -> a -> Bool
    f x = \y -> x > y && x `mod` 3 == y `mod` 7
    --(e)
    g :: [(a, b, c)] -> [c]
    g xs = map third xs

-- Question 2
    --(a)
    countOdd :: Integral a => [a] -> Int
    countOdd [] = 0
    countOdd (x:xs) | odd x = 1 + countOdd xs
                    | otherwise = countOdd xs

    --(b)
    countProperty :: (a -> Bool) -> [a] -> Int
    countProperty p [] = 0
    countProperty p (x:xs)  | p x = 1 + countProperty p xs
                            | otherwise = countProperty p xs

    --(c)
    splitByY :: Ord a => [a] -> a -> ([a], [a], [a])
    splitByY xs y = (lessthan, equalto, morethan)
        where
            lessthan = [ x | x <- xs, x < y ]
            equalto = [ x | x <- xs, x == y]
            morethan = [ x | x <- xs, x > y]

    --(d)
    isSorted :: Ord a => [a] -> Bool
    isSorted [] = True
    isSorted [x] = True
    isSorted (x:y:xys)      | x < y = True && isSorted (y:xys)
                            | otherwise = False

    --(e)
    quickSort :: Ord a => [a] -> [a]
    quickSort [] = []
    quickSort [x] = [x]
    quickSort (x:xs) = (quickSort lessthanx) ++ [x] ++ (quickSort greaterthanx)
        where
            lessthanx = [ y | y <- xs, y <= x ]
            greaterthanx = [ y | y <- xs, y > x]

    --(f)
    listSum :: Num a => [a] -> a
    listSum = foldr (+) 0

-- Question 3
    data BT a = Leaf a | Fork (BT a) (BT a) deriving Show

    data Expr   = Value Integer
                | Add Expr Expr
                | Sub Expr Expr
                | Mul Expr Expr
                | Div Expr Expr

    --(a)
    data RT a = RTLeaf a | RTFork [RT a]

    --(b)
    btElems :: (BT a) -> [a]
    btElems (Leaf x) = [x]
    btElems (Fork left right) = btElems left ++ btElems right

    --(c)
    eval :: Expr -> Double
    eval (Value x) = fromIntegral x
    eval (Add left right) = (eval left) + (eval right)
    eval (Sub left right) = (eval left) - (eval right)
    eval (Mul left right) = (eval left) * (eval right)
    eval (Div left right) = (eval left) / (eval right)

    main :: IO ()
    main = do
        putStrLn "Please enter a String"
        str1 <- getLine
        putStrLn "Please enter an additional String"
        str2 <- getLine
        putStrLn (str1 ++ str2)