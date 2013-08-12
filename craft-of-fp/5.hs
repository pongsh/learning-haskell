import Data.Char (isLower, toUpper)
import Prelude hiding (elem)

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

elem :: Int -> [Int] -> Bool
elem n xs = not $ matches n xs == []
