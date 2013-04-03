data MorseUnit = Beep | Silence     deriving (Eq, Show)

dit, dah, shortGap, mediumGap :: [MorseUnit]
dit         = [Beep, Silence]
dah         = [Beep, Beep, Beep, Silence]
shortGap    = replicate (3-1) Silence
mediumGap   = replicate (7-3) Silence

codeSymbol :: Char -> [MorseUnit]
codeSymbol 'A' = dit ++ dah
codeSymbol 'B' = dah ++ dit ++ dit ++ dit
codeSymbol 'C' = dah ++ dit ++ dah ++ dit
codeSymbol 'D' = dah ++ dit ++ dit
codeSymbol 'E' = dit
codeSymbol 'F' = dit ++ dit ++ dah ++ dit
codeSymbol 'G' = dah ++ dah ++ dit
codeSymbol 'H' = dit ++ dit ++ dit ++ dit
codeSymbol 'I' = dit ++ dit
codeSymbol 'J' = dit ++ dah ++ dah ++ dah
codeSymbol 'K' = dah ++ dit ++ dah
codeSymbol 'L' = dit ++ dah ++ dit ++ dit
codeSymbol 'M' = dah ++ dah
codeSymbol 'N' = dah ++ dit
codeSymbol 'O' = dah ++ dah ++ dah
codeSymbol 'P' = dit ++ dah ++ dah ++ dit
codeSymbol 'Q' = dah ++ dah ++ dit ++ dah
codeSymbol 'R' = dit ++ dah ++ dit
codeSymbol 'S' = dit ++ dit ++ dit
codeSymbol 'T' = dah
codeSymbol 'U' = dit ++ dit ++ dah
codeSymbol 'V' = dit ++ dit ++ dit ++ dah
codeSymbol 'W' = dit ++ dah ++ dah
codeSymbol 'X' = dah ++ dit ++ dit ++ dah
codeSymbol 'Y' = dah ++ dit ++ dah ++ dah
codeSymbol 'Z' = dah ++ dah ++ dit ++ dit

codeSymbol '1' = dit ++ dah ++ dah ++ dah ++ dah
codeSymbol '2' = dit ++ dit ++ dah ++ dah ++ dah
codeSymbol '3' = dit ++ dit ++ dit ++ dah ++ dah
codeSymbol '4' = dit ++ dit ++ dit ++ dit ++ dah
codeSymbol '5' = dit ++ dit ++ dit ++ dit ++ dit
codeSymbol '6' = dah ++ dit ++ dit ++ dit ++ dit
codeSymbol '7' = dah ++ dah ++ dit ++ dit ++ dit
codeSymbol '8' = dah ++ dah ++ dah ++ dit ++ dit
codeSymbol '9' = dah ++ dah ++ dah ++ dah ++ dit
codeSymbol '0' = dah ++ dah ++ dah ++ dah ++ dah

codeWord :: String -> [MorseUnit]
codeWord = foldr f z
    where   f x y = (codeSymbol x) ++ shortGap ++ y
            z = []
            
codeText :: String -> [MorseUnit]
codeText = foldr f z . words
        where   f x y = (codeWord x) ++ mediumGap ++ y
                z = []

lookupMorse :: [MorseUnit] -> String
lookupMorse ms = [ c | (m,c) <- table , m == ms ]
                
table :: [([MorseUnit], Char)]
table = [(mediumGap, ' ')
        ,(dit ++ dah ++ shortGap, 'A')
        ,(dah ++ dit ++ dit ++ dit ++ shortGap, 'B')
        ,(dah ++ dit ++ dah ++ dit ++ shortGap, 'C')
        ,(dah ++ dit ++ dit ++ shortGap, 'D')
        ,(dit ++ shortGap, 'E')
        ,(dit ++ dit ++ dah ++ dit ++ shortGap, 'F')
        ,(dah ++ dah ++ dit ++ shortGap, 'G')
        ,(dit ++ dit ++ dit ++ dit ++ shortGap, 'H')
        ,(dit ++ dit ++ shortGap, 'I')
        ,(dit ++ dah ++ dah ++ dah ++ shortGap, 'J')
        ,(dah ++ dit ++ dah ++ shortGap, 'K')
        ,(dit ++ dah ++ dit ++ dit ++ shortGap, 'L')
        ,(dah ++ dah ++ shortGap, 'M')
        ,(dah ++ dit ++ shortGap, 'N')
        ,(dah ++ dah ++ dah ++ shortGap, 'O')
        ,(dit ++ dah ++ dah ++ dit ++ shortGap, 'P')
        ,(dah ++ dah ++ dit ++ dah ++ shortGap, 'Q')
        ,(dit ++ dah ++ dit ++ shortGap, 'R')
        ,(dit ++ dit ++ dit ++ shortGap, 'S')
        ,(dah ++ shortGap, 'T')
        ,(dit ++ dit ++ dah ++ shortGap, 'U')
        ,(dit ++ dit ++ dit ++ dah ++ shortGap, 'V')
        ,(dit ++ dah ++ dah ++ shortGap, 'W')
        ,(dah ++ dit ++ dit ++ dah ++ shortGap, 'X')
        ,(dah ++ dit ++ dah ++ dah ++ shortGap, 'Y')
        ,(dah ++ dah ++ dit ++ dit ++ shortGap, 'Z')
        ,(dit ++ dah ++ dah ++ dah ++ dah ++ shortGap, '1')
        ,(dit ++ dit ++ dah ++ dah ++ dah ++ shortGap, '2')
        ,(dit ++ dit ++ dit ++ dah ++ dah ++ shortGap, '3')
        ,(dit ++ dit ++ dit ++ dit ++ dah ++ shortGap, '4')
        ,(dit ++ dit ++ dit ++ dit ++ dit ++ shortGap, '5')
        ,(dah ++ dit ++ dit ++ dit ++ dit ++ shortGap, '6')
        ,(dah ++ dah ++ dit ++ dit ++ dit ++ shortGap, '7')
        ,(dah ++ dah ++ dah ++ dit ++ dit ++ shortGap, '8')
        ,(dah ++ dah ++ dah ++ dah ++ dit ++ shortGap, '9')
        ,(dah ++ dah ++ dah ++ dah ++ dah ++ shortGap, '0')]