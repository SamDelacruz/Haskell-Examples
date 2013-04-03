import Test.QuickCheck

split :: [a] -> ([a],[a])
split [] = ([] , [])
split [x] = ([x] , [])
split (x:y:xys) = (x:xs, y:ys) where (xs, ys) = split xys

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) |   x > y = y:merge (x:xs) ys
                    |   otherwise = x:merge xs (y:ys)
                    
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort ls) (mergeSort rs) where (ls, rs) = split xs

sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted [x] = True
sorted (x:y:xys)    |   x > y       =   False
                    |   otherwise   =   True && sorted (y:xys)
                    
prop_mergeSortSorted :: [Int] -> Bool
prop_mergeSortSorted xs = sorted (mergeSort xs)

prop_mergeSortLength :: [Int] -> Bool
prop_mergeSortLength xs = length xs == length (mergeSort xs)

prop_mergeSortQuickSort :: [Int] -> Bool
prop_mergeSortQuickSort xs = mergeSort xs == quickSort xs

test_mergeSort :: IO ()
test_mergeSort = do
    quickCheck prop_mergeSortSorted
    quickCheck prop_mergeSortLength
    quickCheck prop_mergeSortQuickSort

quickSort :: Ord a => [a] -> [a]
quickSort []     = []
quickSort (p:xs) = (quickSort lesser) ++ [p] ++ (quickSort greater)
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs