import Prelude hiding (maximum, gcd)
import Data.List (nub)

-- Ex 4.1
maxThree :: Int -> Int -> Int -> Int
maxThree x y z = max (max x y) z

maxFour :: Int -> Int -> Int -> Int -> Int
maxFour a b c d = max a (maxThree b c d)

maxFour' :: Int -> Int -> Int -> Int -> Int
maxFour' a b c d = max (max (max a b) c) d

-- Ex 4.2
between :: Int -> Int -> Int -> Bool
between x y z = x <= y && y <= z

-- Ex 4.3
howManyEqual :: Int -> Int -> Int -> Int
howManyEqual x y z
  | x == y && y == z = 3
  | x == y && x /= z = 2
  | x == z && y /= x = 2
  | otherwise        = 0

-- Ex 4.5
rangeProduct :: Int -> Int -> Int
rangeProduct m n
  | m > n     = 0
  | m == n    = n
  | otherwise = m * rangeProduct (m+1) n 

-- Ex 4.6
fac :: Int -> Int
fac n 
  | n > 0     = 0 
  | otherwise = rangeProduct 1 n

-- Ex 4.7
multiply :: Int -> Int -> Int
multiply x y
  | x == 1    = y
  | otherwise = y + multiply (x-1) y

-- Ex 4.8
integerSqrt :: Int -> Int
integerSqrt n = integerSqrtHelper n 1

integerSqrtHelper n a
  | a*a > n   = a-1  
  | otherwise = integerSqrtHelper n (a+1)

-- Ex 4.9
maximum :: (Int -> Int) -> Int -> Int
maximum f n 
  | n == 0    = f 0
  | otherwise = max (f n) (maximum f (n-1))

f 0 = 0
f 1 = 44
f 2 = 17
f 3 = 60
f 4 = 50
f 7 = 100
f _ = 0

-- Ex 4.10
anyZero :: (Int -> Int) -> Int -> Bool
anyZero f n
  | n == 0    = f 0 == 0
  | otherwise = f n == 0 || anyZero f (n-1)

-- Ex 4.11
sumFun :: (Int -> Int) -> Int -> Int
sumFun f n
  | n == 0 = f 0
  | n > 0  = sumFun f (n-1) + f n

regions :: Int -> Int
regions n = sumFun (max 1) n

--Ex 4.13
gcd:: Int -> Int -> Int
gcd a b
  | b == 0    = a
  | otherwise = gcd b (a `mod` b)
