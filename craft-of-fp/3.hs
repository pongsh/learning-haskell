import Prelude hiding (min)
import Data.Char (ord, isDigit)

exclusiveOr :: Bool -> Bool -> Bool
exclusiveOr x y = (x && not y) || (not x && y)

nAnd :: Bool -> Bool -> Bool
nAnd x y = not (x && y)

threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent a b c = a /= b && b /= c && a /= c

fourEqual :: Int -> Int -> Int -> Int -> Bool
fourEqual a b c d = not (threeDifferent a b c) && d == a && d == b && d == c

min :: Int -> Int -> Int
min x y = if x > y then y else x

minThree :: Int -> Int -> Int -> Int
minThree x y z = 
  let minTwo = min x y
  in if minTwo < z then minTwo else z

charToNum :: Char -> Int
charToNum x = if isDigit x then ord x - 48 else 0

numberNDroots :: Float -> Float -> Float -> Int
numberNDroots a b c
  | b*b >  4.0*a*c = 2
  | b*b == 4.0*a*c = 1
  | b*b <  4.0*a*c = 0

numberRoots :: Float -> Float -> Float -> Int
numberRoots a b c 
  | a /= 0               = numberNDroots a b c
  | b /= 0.0             = 1
  | b == 0.0 && c /= 0.0 = 0
  | otherwise            = 3

smallerRoot :: Float -> Float -> Float -> Float
smallerRoot a b c
  | numRoots == 0 || numRoots == 3 = 0
  | otherwise                      = ((-b) - sqrt (b*b - 4*a*c)) / (2*a)
  where numRoots = numberRoots a b c

largerRoot :: Float -> Float -> Float -> Float
largerRoot a b c = ((-b) + sqrt (b*b - 4*a*c)) / (2*a)
