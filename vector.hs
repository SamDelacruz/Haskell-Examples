data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

dotProd :: (Num a) => Vector a -> Vector a -> a
(Vector i j k) `dotProd` (Vector l m n) = i*l + j*m + k*n

vmult :: (Num a) => Vector a -> a -> Vector a
(Vector i j k) `vmult` m = Vector (i*m) (j*m) (k*m)

crossProd :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `crossProd` (Vector l m n) = Vector (j*n - k*m) (k*l - i*n) (i*m - j*l)

magnitude :: Vector Double -> Double
magnitude = (\(Vector x y z) -> sqrt $ (x^2) + (y^2) + (z^2) )