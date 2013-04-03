data Atom = Num Integer
        |   Add
        |   Mul
        |   Sub
        deriving (Eq, Show)

rpn :: [Integer] -> [Atom] -> Maybe Integer
-- Can't return a value when given nothing
rpn [] [] = Nothing
-- List of Atoms is exhausted, so return final value
rpn (x:xs) [] = Just x
-- Add pops two integers, and pushes their sum
rpn (x:y:xys) (Add:as) = rpn ((x + y):xys) as
-- Mul pops two integers, and pushes their product
rpn (x:y:xys) (Mul:as) = rpn ((x * y):xys) as
-- Sub pops two integers, and pushes their difference
rpn (x:y:xys) (Sub:as) = rpn ((x - y):xys) as
-- Num pushes an integer onto the stack
rpn xs ((Num a):as) = rpn (a:xs) as
-- Catchall if none of the above patterns match
rpn _ _ = Nothing

eval :: [Atom] -> Maybe Integer
eval = rpn []