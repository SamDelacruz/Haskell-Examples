-- Huffman coding in Haskell.
--
-- Based on Richard Bird's book
-- "Introduction to Functional Programming using Haskell".
--
-- See also the Wikipedia page "Huffman coding".

module Main where

import Data.Char
import Data.Function
import Data.List(sortBy, find)
import Data.Maybe
import Data.Ord
import Test.QuickCheck hiding (sample)

testString = "Huffman coding uses a specific method for choosing the representation for each symbol, resulting in a prefix code (sometimes called prefix-free codes, that is, the bit string representing some particular symbol is never a prefix of the bit string representing any other symbol) that expresses the most common source symbols using shorter strings of bits than are used for less common source symbols. Huffman was able to design the most efficient compression method of this type: no other mapping of individual source symbols to unique strings of bits will produce a smaller average output size when the actual symbol frequencies agree with those used to create the code. A method was later found to design a Huffman code in linear time if input probabilities (also known as weights) are sorted.[citation needed] For a set of symbols with a uniform probability distribution and a number of members which is a power of two, Huffman coding is equivalent to simple binary block encoding, e.g., ASCII coding. Huffman coding is such a widespread method for creating prefix codes that the term Huffman code is widely used as a synonym for prefix code even when such a code is not produced by Huffman's algorithm. Although Huffman's original algorithm is optimal for a symbol-by-symbol coding (i.e. a stream of unrelated symbols) with a known input probability distribution, it is not optimal when the symbol-by-symbol restriction is dropped, or when the probability mass functions are unknown, not identically distributed, or not independent (e.g., cat is more common than cta). Other methods such as arithmetic coding and LZW coding often have better compression capability: both of these methods can combine an arbitrary number of symbols for more efficient coding, and generally adapt to the actual input statistics, the latter of which is useful when input probabilities are not precisely known or vary significantly within the stream. However, the limitations of Huffman coding should not be overstated; it can be used adaptively, accommodating unknown, changing, or context-dependent probabilities. In the case of known independent and identically distributed random variables, combining symbols reduces inefficiency in a way that approaches optimality as the number of symbols combined increases."

test :: [Char] -> Bool
test xs = xs == decompress(compress xs)

data Bit = Zero | One deriving (Show)

compress :: [Char] -> (Btree Char, [Bit])
compress xs = (h , encode t xs)
 where s = sample xs
       h = unlabel(mkHuff s)
       t = transform h

decompress :: (Btree Char,[Bit]) -> [Char]
decompress (t,xs) = decode t xs

compressRatio :: [Char] -> Float
compressRatio xs = fromIntegral(length bs) / fromIntegral(8 * length xs)
  where (t,bs) = compress xs

data Btree a = Leaf a | Fork (Btree a) (Btree a)
             deriving (Show)

decode :: Btree Char -> [Bit] -> [Char]
decode t cs =
  if null cs then [] else decode' t cs
  where decode' (Leaf x) cs              = x : decode t cs
        decode' (Fork xt yt) (Zero : cs) = decode' xt cs
        decode' (Fork xt yt) (One  : cs) = decode' yt cs

type CodeTable = [(Char,[Bit])]

tableLookup :: CodeTable -> Char -> [Bit]
tableLookup ((x,bs):xbs) y = if x == y then bs else tableLookup xbs y

encode :: CodeTable -> [Char] -> [Bit]
encode t = concat . map(tableLookup t)
-- encode t xs = concat [ tableLookup t x | x <- xs]

transform :: Btree Char -> CodeTable
transform (Leaf x) = [(x,[])]
transform (Fork xt yt) = hufmerge (transform xt) (transform yt)

hufmerge :: CodeTable -> CodeTable -> CodeTable
hufmerge [] ycs = [(y,One:cs)  | (y,cs) <- ycs ]
hufmerge xbs [] = [(x,Zero:cs) | (x,cs) <- xbs ]
hufmerge ((x, bs) : xbs) ((y , cs) : ycs)
 | length bs <= length cs = (x,Zero : bs) : hufmerge xbs ((y , cs) : ycs)
 | otherwise              = (y,One  : cs) : hufmerge ((x, bs) : xbs) ycs

sample :: [Char] -> [(Char,Int)]
sample = sortby freq . collate . sortby id

-- NB. id is the "identity function"
-- id :: a -> a
-- id x = x

freq :: (Char,Int) -> Int
freq (x,m) = m

--sortby :: Ord b => (a -> b) -> [a] -> [a]
--sortby = quickSortBy

quickSortBy :: Ord b => (a -> b) -> [a] -> [a]
quickSortBy f [] = []
quickSortBy f (x : xs) =  quickSortBy f [ y | y <- xs, f y < f x]
                       ++ [x]
                       ++ quickSortBy f [ y | y <- xs, f y >= f x]

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x : xs) =    quickSort [ y | y <- xs, y < x]
                     ++ [x]
                     ++ quickSort [ y | y <- xs, y >= x]

collate :: [Char] -> [(Char,Int)]
collate [] = []
collate (x : xs) = (x, 1 + length ys) : collate zs
 where (ys,zs) = span (==x) (x : xs)

-- span p xs = (takeWhile p xs, dropWhile p xs)

data Huff = Tip Int Char | Node Int Huff Huff deriving (Show)

unlabel :: Huff -> Btree Char
unlabel (Tip w x) = Leaf x
unlabel (Node w xt yt) = Fork (unlabel xt) (unlabel yt)

mkHuff :: [(Char,Int)] -> Huff
mkHuff = unwrap . until singleton combine . map mktip

singleton :: [a] -> Bool
singleton xs = length xs == 1

unwrap :: [a] -> a
unwrap [x] = x

mktip :: (Char,Int) -> Huff
mktip (c,w) = Tip w c

combine :: [Huff] -> [Huff]
combine (xt : yt : xts) = insert (Node w xt yt) xts
 where w = weight xt + weight yt

weight :: Huff -> Int
weight (Tip w c) = w
weight (Node w x y) = w

insert :: Huff -> [Huff] -> [Huff]
insert xt yts = uts ++ [xt] ++ vts
 where (uts,vts) = span p yts
       p yt = (weight yt < weight xt)

main :: IO ()
main = print (map (sum . map ord . decompress . compress) [take n testString | n <- [10..5000]])

-------------------------------------------------------------------------------
-- YOUR SOLUTIONS START HERE. Comment out the original functions when you make
-- new versions.
sortby :: Ord b => (a -> b) -> [a] -> [a]
sortby f xs = sortBy g where g = f xs