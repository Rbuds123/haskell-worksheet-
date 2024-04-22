import Data.Char
import Prelude hiding(reverse)

twice :: (Int -> Int) -> Int -> Int
twice f x = f (f x)

multiply :: Int -> Int -> Int
multiply x y = x * y

double :: Int -> Int
double = multiply 2

doubleAll :: [Int] -> [Int]
doubleAll = map (*2)

areDigits :: String -> [Bool]
areDigits = map isDigit

keepPositive :: [Int] -> [Int]
keepPositive = filter (>0)

keepDigits :: String -> String
keepDigits = filter isDigit

addUp :: [Int] -> Int
addUp = foldr (+) 0 

myConcat :: [[a]] -> [a]
myConcat = foldr (++) []
----------------------------------------------
mult10 :: [Int] -> [Int]
mult10 = map (* 10)

onlyLowerCase :: String -> String
onlyLowerCase = filter isLower

orAll :: [Bool] -> Bool
orAll = foldr (||) False

sumSquares :: [Int] -> Int
sumSquares = sum . map (^2)

filterBetweenZeroAndTen :: Int -> Bool
filterBetweenZeroAndTen x = x >= 0 && x <= 10

zeroToTen :: [Int] -> [Int]
zeroToTen = filter (>= 0) . filter (<= 10)

squareRoots :: [Float] -> [Float]
squareRoots = map sqrt . filter (>= 0)

countBetween :: Float -> Float -> [Float] -> Int
countBetween lower upper = length . filter (\x -> x >= lower && x <= upper)

alwaysPositive :: (Float -> Float) -> [Float] -> Bool
alwaysPositive f = all ((> 0) . f)

productSquareRoots :: [Float] -> Float
productSquareRoots = foldr multiplyIfNonNegative 1.0
  where multiplyIfNonNegative x acc = if x >= 0 then sqrt x * acc else acc

alwaysPositive' :: (Float -> Float) -> [Float] -> Bool
alwaysPositive' f = all (\x -> f x > 0)

alwaysPositive'' :: (Float -> Float) -> [Float] -> Bool
alwaysPositive'' f = foldr (\x acc -> f x > 0 && acc) True

removeFirst :: (a -> Bool) -> [a] -> [a]
removeFirst p (x : xs)
      |p x = xs
      |otherwise = x : removeFirst p xs

removeLast :: (a -> Bool) -> [a] -> [a]
removeLast p lst = reverse (removeFirst p (reverse lst))


zeroToTen' :: [Int] -> [Int]
zeroToTen' = filter (\x -> x >= 0 && x <= 10)

alwaysPositive''' :: (Float -> Float) -> [Float] -> Bool
alwaysPositive''' f = foldr (\x acc -> f x > 0 && acc) True

productSquareRoots' :: [Float] -> Float
productSquareRoots' = foldr (\x acc -> if x >= 0 then sqrt x * acc else acc) 1.0

reverse :: [a] -> [a]
reverse = foldr (\x acc -> acc ++ [x]) []