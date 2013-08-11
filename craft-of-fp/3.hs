import Prelude hiding (min)
import Data.Char (chr, ord, isDigit)

-- Ex 3.1
exclusiveOr :: Bool -> Bool -> Bool
exclusiveOr x y = (x && not y) || (not x && y)

-- Ex 3.2
nAnd :: Bool -> Bool -> Bool
nAnd x y = not (x && y)

-- Ex 3.7
threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent a b c = a /= b && b /= c && a /= c

-- Ex 3.8
fourEqual :: Int -> Int -> Int -> Int -> Bool
fourEqual a b c d = not (threeDifferent a b c) && d == a && d == b && d == c

-- Ex 3.11
min :: Int -> Int -> Int
min x y = if x > y then y else x

minThree :: Int -> Int -> Int -> Int
minThree x y z = 
  let minTwo = min x y
  in if minTwo < z then minTwo else z

-- Ex 3.12
toUpper :: Char -> Char
toUpper ch 
  | ord ch >= ord 'a' && ord ch <= ord 'z' = chr (ord ch + (ord 'A' - ord 'a'))
  | otherwise = ch

-- Ex 3.13
charToNum :: Char -> Int
charToNum x = if isDigit x then ord x - 48 else 0

-- Ex 3.14
averageThree :: Int -> Int -> Int -> Float
averageThree a b c = fromIntegral (a+b+c) / 3

howManyAboveAverage :: Int -> Int -> Int -> Int
howManyAboveAverage a b c = length (filter (\x -> fromIntegral x > averageThree a b c) [a,b,c])

-- Ex 3.15
numberNDroots :: Float -> Float -> Float -> Int
numberNDroots a b c
  | b*b >  4.0*a*c = 2
  | b*b == 4.0*a*c = 1
  | b*b <  4.0*a*c = 0

-- Ex 3.16
numberRoots :: Float -> Float -> Float -> Int
numberRoots a b c 
  | a /= 0               = numberNDroots a b c
  | b /= 0.0             = 1
  | b == 0.0 && c /= 0.0 = 0
  | otherwise            = 3

-- Ex 3.17
smallerRoot :: Float -> Float -> Float -> Float
smallerRoot a b c
  | numRoots == 0 || numRoots == 3 = 0
  | otherwise                      = ((-b) - sqrt (b*b - 4*a*c)) / (2*a)
  where numRoots = numberRoots a b c

largerRoot :: Float -> Float -> Float -> Float
largerRoot a b c 
  | numRoots == 0 || numRoots == 3 = 0
  | otherwise                      = ((-b) + sqrt (b*b - 4*a*c)) / (2*a)
  where numRoots = numberRoots a b c

