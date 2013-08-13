import Data.Char (isLower, toUpper)

-- Ex 5.1
maxOccurs :: Int -> Int -> (Int, Int)
maxOccurs x y
  | x == y    = (x, 2)
  | x > y     = (x, 1)
  | otherwise = (y, 1)

maxThreeOccurs :: Int -> Int -> Int -> (Int, Int)
maxThreeOccurs x y z
  | m == z     = (m, n+1)
  | m > z      = (m, n)
  | otherwise  = (z, 1)
  where m = fst $ maxOccurs x y
        n = snd $ maxOccurs x y

-- Ex 5.3
-- ax + by = c
crossXAxis :: Float -> Float -> Float -> Float
crossXAxis a b c
  | a /= 0    = c/a
  | otherwise = 0

-- Ex 5.8
doubleAll :: [Int] -> [Int]
doubleAll xs = [x*2 | x <- xs]

-- Ex 5.9
capitalize :: String -> String
capitalize str = [toUpper x | x <- str]

capitalizeLetters :: String -> String
capitalizeLetters str = [toUpper x | x <- str, isLower x]

-- Ex 5.10
divisors :: Int -> [Int]
divisors n = [x | x <- [1..n], n `mod` x == 0]

isPrime :: Int -> Bool
isPrime n = divisors n == [1, n]   

-- Ex 5.11
matches :: Int -> [Int] -> [Int]
matches n xs = [x | x <- xs, x == n]

elem' :: Int -> [Int] -> Bool
elem' n xs = not $ matches n xs == []

-- Ex 5.13

type Person = String
type Book   = String

type Database = [ (Person, Book) ]

exampleBase :: Database
exampleBase = [ ("Alice", "Tintin"), ("Anna", "Little Women"),
                ("Alice", "Asterix"), ("Rory", "Tintin") ]

books :: Database -> Person -> [Book]
books dBase findperson = [ book | (person, book) <- dBase, findperson == person ]

makeLoan :: Database -> Person -> Book -> Database
makeLoan dBase pers bk = [ (pers, bk) ] ++ dBase

returnLoan :: Database => Person -> Book -> Database
returnLoan dBase pers bk = [ pair | pair <- dBase, pair /= (pers, bk) ]

borrowers :: Database -> Book -> [Person] 
borrowers dBase bk = [ person | (person, book) <- dBase, bk == book ]

borrowed :: Database -> Book -> Bool
borrowed dBase bk = not (null (borrowers dBase bk))

numBorrowed :: Database -> Person -> Int
numBorrowed dBase pers = length $ books dBase pers

-- Ex 5.16
-- snd :: (a, b) -> b
-- sing :: a -> [a]

-- Ex 5.19
stringToUpper :: String -> String
stringToUpper str = [ toUpper ch | ch <- str ]

-- Ex 5.20
romanDigit :: Char -> String
romanDigit ch = 
  case ch of
    '1' -> "I"
    '2' -> "II"
    '3' -> "III"
    '4' -> "IV"
    '5' -> "V"
    '6' -> "VI"
    '7' -> "VII"
    '8' -> "VIII"
    '9' -> "IX"

-- Ex 5.21
onThreeLines :: String -> String -> String -> String
onThreeLines str1 str2 str3 = str1 ++ "\n" ++ str2 ++ "\n" ++ str3 ++ "\n"

-- Ex 5.22
onSeparateLines :: [String] -> String
onSeparateLines xs = concat [ x ++"\n" | x <- xs ]

-- Ex 5.23
duplicate :: String -> Int -> String
duplicate str n
  | n <= 0 = ""
  | n == 1 = str
  | otherwise = str ++ duplicate str (n-1)

-- Ex 5.24
linelength :: Int
linelength = 12

pushRight :: String -> String
pushRight str = duplicate " " (linelength - length str)  ++ str 

-- Ex 5.25
fib :: Int -> Int
fib n
  | n <= 1 = n
  | otherwise = fib (n-1) + fib (n-2)

fibTable :: Int -> String
fibTable n = pushRight "n" ++ pushRight "fib n" ++ "\n" ++ onSeparateLines table
             where table = [pushRight (show x) ++ pushRight (show (fib x)) | x <- [0..n]]
